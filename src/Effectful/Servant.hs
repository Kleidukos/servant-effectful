{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Effectful.Servant where

import Effectful
import Effectful.Error.Static
import Servant.Server (ServerError, ServerT, Application, HasServer, Handler, Server, hoistServer, ServerContext, Context)
import Effectful.Dispatch.Static
import qualified Control.Monad.Except as T
import qualified Servant.Server as Servant
import Data.Proxy (Proxy(..))
import Data.Kind (Type)
import Servant.Server.Generic (AsServerT)
import Servant.API.Generic (GenericServant, AsApi, ToServantApi, ToServant)
import qualified Network.Wai.Handler.Warp as Warp

data Servant :: Effect

type instance DispatchOf Servant = Static WithSideEffects
newtype instance StaticRep Servant = Servant Warp.Settings

-- | Run a 'Servant' computation with the specified settings
runServant :: IOE :> es
           => Warp.Settings -- ^ Settings for the webserver
           -> Eff (Servant : es) a -- ^ The computation to run
           -> Eff es a
runServant settings = evalStaticRep (Servant settings)

runWarpServerSettings :: forall (es :: [Effect]) (api :: Type).
                         (Servant :> es, Error ServerError :> es, IOE :> es)
                      => ServerT api (Eff es) -> Eff es ()
runWarpServerSettings server = do
  Servant settings <- getStaticRep
  unsafeEff_ $ Warp.runSettings settings (serveEff @es @api server)

serveEff :: forall (es :: [Effect]) (api :: Type).
            (HasServer api '[], Error ServerError :> es, IOE :> es)
         => ServerT api (Eff es)
         -> Application
serveEff computation = do
  let api = Proxy @api
  Servant.serve api (Servant.hoistServer api effToHandler computation)

effToHandler :: forall (es :: [Effect]) (a :: Type).
                (Error ServerError :> es, IOE :> es)
             => Eff es a  -> Handler a
effToHandler computation = do
  v <- liftIO . runEff . runErrorNoCallStack @ServerError $ computation
  either T.throwError pure v

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
