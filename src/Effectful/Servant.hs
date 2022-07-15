{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Effectful.Servant where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Effectful
import Effectful.Dispatch.Static
import Effectful.Error.Static
import Servant.API.Generic (GenericServant, AsApi, ToServantApi, ToServant)
import Servant.Server (ServerError, ServerT, Application, HasServer, Handler, Server, hoistServer, ServerContext, Context)
import Servant.Server.Generic (AsServerT)
import qualified Control.Monad.Except as T
import qualified Network.Wai.Handler.Warp as Warp
import qualified Servant.Server as Servant

data Servant :: Effect

type instance DispatchOf Servant = Static WithSideEffects

runWarpServerSettings :: forall (es :: [Effect]) (api :: Type). (HasServer api '[])
                      => Warp.Settings
                      -> ServerT api (Eff [Error ServerError, IOE])
                      -> Eff es ()
runWarpServerSettings settings server = do
  unsafeEff_ $ Warp.runSettings settings (serveEff @api server)

serveEff :: forall (api :: Type). (HasServer api '[])
         => ServerT api (Eff '[Error ServerError, IOE])
         -> Application
serveEff computation = do
  let api = Proxy @api
   in Servant.serve api (Servant.hoistServer @api api effToHandler computation)

effToHandler :: forall (a :: Type). ()
             => Eff '[Error ServerError, IOE] a
             -> Handler a
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
