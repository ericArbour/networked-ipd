{-# LANGUAGE OverloadedStrings #-}

module Lib.Client
  ( runClient
  ) where

import Control.Concurrent
import Control.Exception (Exception, finally, throw)
import Control.Monad (forever, when)
import Control.Monad.Trans (liftIO)
import Data.Maybe (fromJust, isNothing)
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable)
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.Socket (withSocketsDo)
import Text.Read (readMaybe)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import qualified Servant.Client as SV
import qualified Streamly as S
import qualified Streamly.Prelude as S

import Lib.Shared (API, Move(..), MoveInfo(..))

-- HTTP
-------------------------------------------------------------------------------
api :: Proxy API
api = Proxy

hoistHTTPClient :: Manager -> SV.Client IO API
hoistHTTPClient manager' =
  SV.hoistClient api (handleError . getIOClient) (SV.client api)
  where
    baseurl = SV.BaseUrl SV.Http "localhost" 8081 ""
    clientEnv :: SV.ClientEnv
    clientEnv = SV.mkClientEnv manager' baseurl
    getIOClient :: SV.ClientM a -> IO (Either SV.ClientError a)
    getIOClient = flip SV.runClientM clientEnv
    handleError :: IO (Either SV.ClientError a) -> IO a
    handleError = fmap (either (error . show) id)

-- Websockets
--------------------------------------------------------------------------------
wsClient :: MVar T.Text -> WS.ClientApp ()
wsClient eventMVar conn = do
  putStrLn "Connected!"
  -- Announce strategy to server
  WS.sendTextData conn ("Default Strategy" :: T.Text)
  flip finally disconnect $
    forever $ do
      msg <- WS.receiveData conn
      putMVar eventMVar msg
  where
    disconnect = WS.sendClose conn ("Bye!" :: T.Text)

getEventStream :: S.SerialT IO T.Text
getEventStream = do
  eventMVar <- liftIO newEmptyMVar
  liftIO . forkIO . withSocketsDo $
    WS.runClient "127.0.0.1" 8082 "/" (wsClient eventMVar)
  S.repeatM . liftIO $ takeMVar eventMVar

-- Game
-------------------------------------------------------------------------------
delay :: Int
delay = 1000000

data GameException
  = InvalidStrategy
  | NoStreamFound
  | CannotParseId T.Text
  deriving (Show, Typeable)

instance Exception GameException

idAssignmentPrefix :: T.Text
idAssignmentPrefix = "Your id is: "

parseId :: T.Text -> Maybe Int
parseId = readMaybe . T.unpack . T.replace idAssignmentPrefix ""

eventHandler :: Int -> SV.Client IO API -> String -> T.Text -> IO String
eventHandler myId postMove gameState event = do
  T.putStrLn event
  threadDelay delay
  postMove $ MoveInfo {userId = myId, move = Defect}
  return gameState

-- Main
--------------------------------------------------------------------------------
runClient :: IO ()
runClient = do
  manager' <- newManager defaultManagerSettings
  let postMove = hoistHTTPClient manager'
  decompS <- S.uncons getEventStream
  case decompS of
    Nothing -> throw NoStreamFound
    Just (firstEvent, s) -> do
      when (not $ idAssignmentPrefix `T.isPrefixOf` firstEvent) $
        throw InvalidStrategy
      case parseId firstEvent of
        Nothing -> throw $ CannotParseId firstEvent
        Just myId ->
          S.foldlM' (eventHandler myId postMove) "Game State Placeholder" s
  return ()
