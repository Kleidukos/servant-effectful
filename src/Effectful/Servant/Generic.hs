{-# LANGUAGE AllowAmbiguousTypes #-}
-- Allow redendudant constraints to require IOE for runWarp helpers.
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Effectful.Servant.Generic
  ( -- * main api
    runWarpServerSettings
  , runWarpServerSettingsContext
  , runWarpServerSettingsSocket
  , runWarpServerSettingsSocketContext

    -- * helpers
  , serveEff
  )
where

import Data.Kind (Type)
import Effectful
import Effectful.Dispatch.Static
import Effectful.Dispatch.Static.Primitive (Env)
import Effectful.Error.Static
import Effectful.Servant (interpretServer)
import qualified Network.Socket as Network
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Servant hiding ((:>))
import Servant.Server.Generic

-- | Deploy an effectful server.
runWarpServerSettings
  :: forall routes (es :: [Effect])
   . (GenericServantConstraints routes '[] (Eff (Error ServerError : es)))
  => Warp.Settings
  -> routes (AsServerT (Eff (Error ServerError : es)))
  -> Wai.Middleware
  -> Eff es ()
runWarpServerSettings settings = runWarpServerSettingsContext settings EmptyContext
{-# INLINEABLE runWarpServerSettings #-}

-- | Deploy an effectful server with a context.
runWarpServerSettingsContext
  :: forall routes (context :: [Type]) (es :: [Effect])
   . (GenericServantConstraints routes context (Eff (Error ServerError : es)))
  => Warp.Settings
  -> Context context
  -> routes (AsServerT (Eff (Error ServerError : es)))
  -> Wai.Middleware
  -> Eff es ()
runWarpServerSettingsContext settings ctx routes middleware = do
  unsafeEff $ \es -> do
    Warp.runSettings settings (middleware $ serveEff es routes ctx)
{-# INLINEABLE runWarpServerSettingsContext #-}

-- | Deploy an effectful server on socket.
runWarpServerSettingsSocket
  :: forall routes (es :: [Effect])
   . (GenericServantConstraints routes '[] (Eff (Error ServerError : es)))
  => Warp.Settings
  -> Network.Socket
  -> routes (AsServerT (Eff (Error ServerError : es)))
  -> Wai.Middleware
  -> Eff es ()
runWarpServerSettingsSocket settings socket routes =
  runWarpServerSettingsSocketContext settings socket routes EmptyContext
{-# INLINEABLE runWarpServerSettingsSocket #-}

-- | Deploy an effectful server on socket with a context.
runWarpServerSettingsSocketContext
  :: forall routes (context :: [Type]) (es :: [Effect])
   . (GenericServantConstraints routes context (Eff (Error ServerError : es)))
  => Warp.Settings
  -> Network.Socket
  -> routes (AsServerT (Eff (Error ServerError : es)))
  -> Context context
  -> Wai.Middleware
  -> Eff es ()
runWarpServerSettingsSocketContext settings socket routes ctx middleware = do
  unsafeEff $ \es -> do
    Warp.runSettingsSocket settings socket (middleware $ serveEff es routes ctx)
{-# INLINEABLE runWarpServerSettingsSocketContext #-}

-- | Convert an effectful server into a wai application.
serveEff
  :: forall routes (context :: [Type]) (es :: [Effect])
   . (GenericServantConstraints routes context (Eff (Error ServerError : es)))
  => Env es
  -> routes (AsServerT (Eff (Error ServerError : es)))
  -> Context context
  -- -> ServerT api ()
  -> Application
serveEff env = genericServeTWithContext (interpretServer env)
{-# INLINEABLE serveEff #-}

type GenericServantConstraints routes ctx m =
  ( GenericServant routes (AsServerT m)
  , GenericServant routes AsApi
  , HasServer (ToServantApi routes) ctx
  , HasContextEntry (ctx .++ DefaultErrorFormatters) ErrorFormatters
  , ServerT (ToServantApi routes) m ~ ToServant routes (AsServerT m)
  )