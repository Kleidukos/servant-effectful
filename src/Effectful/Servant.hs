{-# LANGUAGE AllowAmbiguousTypes #-}
-- Allow redendudant constraints to require IOE for runWarp helpers.
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Effectful.Servant
  ( -- * main api
    runWarpServerSettings
  , runWarpServerSettingsContext

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
import qualified Network.Wai.Handler.Warp as Warp
import Servant hiding ((:>))

-- | Deploy an effectful server.
runWarpServerSettings
  :: forall (api :: Type) (es :: [Effect])
   . (HasServer api '[], IOE :> es)
  => Warp.Settings
  -> ServerT api (Eff (Error ServerError : es))
  -> Eff es ()
runWarpServerSettings settings =
  runWarpServerSettingsContext @api settings EmptyContext

-- | Deploy an effectful server with a context.
runWarpServerSettingsContext
  :: forall (api :: Type) (context :: [Type]) (es :: [Effect])
   . (HasServer api context, ServerContext context, IOE :> es)
  => Warp.Settings
  -> Context context
  -> ServerT api (Eff (Error ServerError : es))
  -> Eff es ()
runWarpServerSettingsContext settings ctx server = do
  unsafeEff $ \es -> do
    Warp.runSettings settings (serveEff @api es ctx server)

-- | Convert an effectful server into a wai application.
serveEff
  :: forall (api :: Type) (context :: [Type]) (es :: [Effect])
   . (HasServer api context, ServerContext context)
  => Env es
  -> Context context
  -> ServerT api (Eff (Error ServerError : es))
  -> Application
serveEff env ctx = Servant.serveWithContextT (Proxy @api) ctx (interpretServer env)

-- | Transform the Eff monad into a servant Handler.
interpretServer :: Env es -> Eff (Error ServerError : es) a -> Servant.Handler a
interpretServer env action = do
  v <- liftIO $ do
    es' <- cloneEnv env
    unEff (runErrorNoCallStack action) es'
  T.liftEither v
