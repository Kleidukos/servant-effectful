{-# LANGUAGE AllowAmbiguousTypes #-}
-- Allow redendudant constraints to require IOE for runWarp helpers.
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Effectful.Servant
  ( -- * main api
    runWarpServerSettings
  , runWarpServerSettingsContext
  , runWarpServerSettingsSocket
  , runWarpServerSettingsSocketContext

    -- * helpers
  , serveEff
  , interpretServer
  )
where

import qualified Control.Monad.Except as T
import Data.Kind (Type)
import Effectful
import Effectful.Dispatch.Static
import Effectful.Dispatch.Static.Primitive (Env, cloneEnv)
import Effectful.Error.Static
import qualified Network.Socket as Network
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Servant hiding ((:>))

-- | Deploy an effectful server.
runWarpServerSettings
  :: forall (api :: Type) (es :: [Effect])
   . (HasServer api '[], IOE :> es)
  => Warp.Settings
  -> ServerT api (Eff (Error ServerError : es))
  -> Wai.Middleware
  -> Eff es ()
runWarpServerSettings settings =
  runWarpServerSettingsContext @api settings EmptyContext
{-# INLINEABLE runWarpServerSettings #-}

-- | Deploy an effectful server with a context.
runWarpServerSettingsContext
  :: forall (api :: Type) (context :: [Type]) (es :: [Effect])
   . (HasServer api context, ServerContext context, IOE :> es)
  => Warp.Settings
  -> Context context
  -> ServerT api (Eff (Error ServerError : es))
  -> Wai.Middleware
  -> Eff es ()
runWarpServerSettingsContext settings ctx server middleware = do
  unsafeEff $ \es -> do
    Warp.runSettings settings (middleware $ serveEff @api es ctx server)
{-# INLINEABLE runWarpServerSettingsContext #-}

-- | Deploy an effectful server on socket.
runWarpServerSettingsSocket
  :: forall (api :: Type) (es :: [Effect])
   . (HasServer api '[], IOE :> es)
  => Warp.Settings
  -> Network.Socket
  -> ServerT api (Eff (Error ServerError : es))
  -> Wai.Middleware
  -> Eff es ()
runWarpServerSettingsSocket settings socket =
  runWarpServerSettingsSocketContext @api settings socket EmptyContext
{-# INLINEABLE runWarpServerSettingsSocket #-}

-- | Deploy an effectful server on socket with a context.
runWarpServerSettingsSocketContext
  :: forall (api :: Type) (context :: [Type]) (es :: [Effect])
   . (HasServer api context, ServerContext context, IOE :> es)
  => Warp.Settings
  -> Network.Socket
  -> Context context
  -> ServerT api (Eff (Error ServerError : es))
  -> Wai.Middleware
  -> Eff es ()
runWarpServerSettingsSocketContext settings socket ctx server middleware = do
  unsafeEff $ \es -> do
    Warp.runSettingsSocket settings socket (middleware $ serveEff @api es ctx server)
{-# INLINEABLE runWarpServerSettingsSocketContext #-}

-- | Convert an effectful server into a wai application.
serveEff
  :: forall (api :: Type) (context :: [Type]) (es :: [Effect])
   . (HasServer api context, ServerContext context)
  => Env es
  -> Context context
  -> ServerT api (Eff (Error ServerError : es))
  -> Application
serveEff env ctx = Servant.serveWithContextT (Proxy @api) ctx (interpretServer env)
{-# INLINEABLE serveEff #-}

-- | Transform the Eff monad into a servant Handler.
interpretServer :: Env es -> Eff (Error ServerError : es) a -> Servant.Handler a
interpretServer env action = do
  v <- liftIO $ do
    es' <- cloneEnv env
    unEff (runErrorNoCallStack action) es'
  T.liftEither v
{-# INLINEABLE interpretServer #-}