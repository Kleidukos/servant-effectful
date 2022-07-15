{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Effectful (Eff, IOE)
import Prelude hiding (lookup)
import Test.Tasty
import Servant
import Data.Text (Text)

import qualified Data.Text as Text
import Data.Word (Word8)
import Servant.API.Generic
import Effectful.Reader.Static (Reader)
import Effectful.Error.Static (Error)

type Api1 = "api" :> "v1" :> "name" :> Get '[JSON] Text

server1 :: ServerT Api1 (Eff es)
server1 = pure $ Text.pack "Carlos"

type Api2 = NamedRoutes Api2'
data Api2' mode = Api2'
  { name :: mode :- "api" :> "v2" :> "name" :> Get '[JSON] Text
  , info :: mode :- "api" :> "v2" :> "info" :> UserInfoApi
  }
  deriving stock Generic

type UserInfoApi = NamedRoutes UserInfoApi'
data UserInfoApi' mode = UserInfoApi'
  { age  :: mode :- "age" :> Get '[JSON] Word8
  , city :: mode :- "city" :> Get '[JSON] Text
  } deriving stock Generic

type MyStack = Eff '[Reader (), Error ServerError, IOE]

server2 :: ServerT Api2 MyStack
server2 = Api2'
  { name = pure $ Text.pack "Carla"
  , info = userInfoServer
  }

userInfoServer :: ServerT UserInfoApi MyStack
userInfoServer = UserInfoApi'
    { age = pure 36
    , city = pure $ Text.pack "Bignasco"
    }

main :: IO ()
main = defaultMain $ testGroup "servant-effectful"
  [ ]
