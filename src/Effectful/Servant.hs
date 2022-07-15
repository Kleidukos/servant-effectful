{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}

module Effectful.Servant where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Effectful
import Effectful.Dispatch.Static
import Effectful.Error.Static
import Servant.Server (ServerError, ServerT, Application, HasServer, Handler, Server, hoistServer)
import qualified Control.Monad.Except as T
import qualified Network.Wai.Handler.Warp as Warp
import qualified Servant.Server as Servant

runWarpServerSettings :: forall (api :: Type) (es :: [Effect]).
                         (HasServer api '[], IOE :> es, Error ServerError :> es)
                      => Warp.Settings
                      -> ServerT api (Eff es)
                      -> Eff es ()
runWarpServerSettings settings server = do
  withEffToIO $ \runInIO -> do
    let api = Proxy @api
        server' = Servant.hoistServer @api api (effToHandlerWith runInIO) server
    Warp.runSettings settings (Servant.serve api server')

serveEff :: forall (api :: Type) (es :: [Effect]).
            (HasServer api '[], Error ServerError :> es)
         => (forall x. Eff es x -> IO x)
         -> ServerT api (Eff es)
         -> Application
serveEff runInIO server = do
  let api = Proxy @api
  Servant.serve api (hoistServerEff api runInIO server)

hoistServerEff :: (HasServer api '[], Error ServerError :> es)
               => Proxy api
               -> (forall x. Eff es x -> IO x)
               -> ServerT api (Eff es)
               -> Server api
hoistServerEff api runInIO = Servant.hoistServer api (effToHandlerWith runInIO)

effToHandler :: forall (a :: Type). ()
             => Eff '[Error ServerError, IOE] a
             -> Handler a
effToHandler computation = do
  v <- liftIO . runEff . runErrorNoCallStack @ServerError $ computation
  either T.throwError pure v

effToHandlerWith :: Error ServerError :> es
                 => (forall x. Eff es x -> IO x)
                 -> Eff es a -> Handler a
effToHandlerWith runInIO
  = Servant.Handler
  . T.withExceptT snd
  . T.ExceptT
  . runInIO
  . tryError

hoistServerIntoEff :: forall (es :: [Effect]) (api :: Type).
                      (HasServer api '[], Error ServerError :> es)
                   => ServerT api Handler -> ServerT api (Eff es)
hoistServerIntoEff = hoistServer (Proxy @api) (handlerToEff @es)

handlerToEff :: forall (es :: [Effect]) (a :: Type).
                (Error ServerError :> es)
             => Handler a
             -> Eff es a
handlerToEff handler = do
  v <- unsafeEff_ $ Servant.runHandler handler
  either throwError pure v
