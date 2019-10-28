{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Lib.Server where

import Prelude ()
import Prelude.Compat

import Control.Concurrent
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import qualified Data.Aeson.Parser
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Types.SourceT (source)
import Streamly
import qualified Streamly.Prelude as S
import System.Directory
import Text.Blaze
import qualified Text.Blaze.Html
import Text.Blaze.Html.Renderer.Utf8

type API = "move" :> ReqBody '[JSON] MoveInfo :> Post '[JSON] MoveInfo

data Move
  = Cooperate
  | Defect
  deriving (Eq, Show, Generic)

instance ToJSON Move

instance FromJSON Move

data MoveInfo =
  MoveInfo
    { userId :: Int
    , move :: Move
    }
  deriving (Eq, Show, Generic)

instance ToJSON MoveInfo

instance FromJSON MoveInfo

server :: MVar MoveInfo -> Server API
server moveMVar = postMove
  where
    postMove :: MoveInfo -> Handler MoveInfo
    postMove moveInfo = do
      liftIO $ putMVar moveMVar moveInfo
      return moveInfo

api :: Proxy API
api = Proxy

app :: MVar MoveInfo -> Application
app moveMVar = serve api (server moveMVar)

getMoveStream :: SerialT IO MoveInfo
getMoveStream = do
  moveMVar <- liftIO newEmptyMVar
  liftIO $ forkIO $ run 8081 (app moveMVar)
  S.repeatM $ liftIO $ takeMVar moveMVar

main :: IO ()
main = do
  runStream $ S.mapM (print) $ getMoveStream
  _ <- getLine
  return ()
