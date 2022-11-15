{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Effectful.Servant where

import qualified Control.Monad.Except as T
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Effectful
import Effectful.Dispatch.Static
import Effectful.Error.Static
import qualified Network.Wai.Handler.Warp as Warp
import Servant.API.Generic (AsApi, GenericServant, ToServant, ToServantApi)
import Servant.Server (Application, Context, Handler, HasServer, Server, ServerContext, ServerError, ServerT, hoistServer)
import qualified Servant.Server as Servant
import Servant.Server.Generic (AsServerT)

type ServerEff api es = ServerT api (Eff es)

runWarpServerSettings ::
    forall (api :: Type) (es :: [Effect]).
    (HasServer api '[], IOE :> es, Error ServerError :> es) =>
    Warp.Settings ->
    ServerEff api es ->
    Eff es ()
runWarpServerSettings settings server = do
    withEffToIO $ \runInIO -> do
        let api = Proxy @api
            server' = Servant.hoistServer @api api (effToHandlerWith runInIO) server
        Warp.runSettings settings (Servant.serve api server')

serveEff ::
    forall (api :: Type) (es :: [Effect]).
    (HasServer api '[], Error ServerError :> es) =>
    (forall x. Eff es x -> IO x) ->
    ServerEff api es ->
    Application
serveEff runInIO server = do
    let api = Proxy @api
    Servant.serve api (hoistServerEff api runInIO server)

hoistServerEff ::
    (HasServer api '[], Error ServerError :> es) =>
    Proxy api ->
    (forall x. Eff es x -> IO x) ->
    ServerEff api es ->
    Server api
hoistServerEff api runInIO = Servant.hoistServer api (effToHandlerWith runInIO)

effToHandler ::
    forall (a :: Type).
    () =>
    Eff '[Error ServerError, IOE] a ->
    Handler a
effToHandler computation = do
    v <- liftIO . runEff . runErrorNoCallStack @ServerError $ computation
    either T.throwError pure v

effToHandlerWith ::
    Error ServerError :> es =>
    (forall x. Eff es x -> IO x) ->
    Eff es a ->
    Handler a
effToHandlerWith runInIO =
    Servant.Handler
        . T.withExceptT snd
        . T.ExceptT
        . runInIO
        . tryError

hoistServerIntoEff ::
    forall (es :: [Effect]) (api :: Type).
    (HasServer api '[], Error ServerError :> es) =>
    ServerT api Handler ->
    ServerT api (Eff es)
hoistServerIntoEff = hoistServer (Proxy @api) (handlerToEff @es)

handlerToEff ::
    forall (es :: [Effect]) (a :: Type).
    (Error ServerError :> es) =>
    Handler a ->
    Eff es a
handlerToEff handler = do
    v <- unsafeEff_ $ Servant.runHandler handler
    either throwError pure v
