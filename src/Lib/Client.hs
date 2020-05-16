{-# LANGUAGE OverloadedStrings #-}

module Lib.Client
  ( runClient
  ) where

import Control.Concurrent
import Control.Exception (Exception, finally, throw)
import Control.Monad (forever, when)
import Control.Monad.Trans (liftIO)
import Data.ByteString.Lazy (ByteString)
import Data.List (intersperse)
import Data.Maybe (fromJust, maybe)
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable)
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.Socket (withSocketsDo)
import Text.Read (readMaybe)

import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import qualified Servant.Client as SV
import qualified Streamly as S
import qualified Streamly.Prelude as S

import Lib.Shared
  ( API
  , Event(..)
  , IdAssignment(..)
  , Move(..)
  , PlayerMove(..)
  , Strategy(..)
  )

type ClientState = ([Event], String)

data GameException
  = InvalidStrategy
  | NoStreamFound
  | CannotParse ByteString
  | InvalidEvent ByteString
  deriving (Show, Typeable)

instance Exception GameException

-- Helpers
-------------------------------------------------------------------------------
formatEventHistory :: [Event] -> String
formatEventHistory = foldr (<>) "" . intersperse "\n" . map show . reverse

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
wsClient :: MVar ByteString -> WS.ClientApp ()
wsClient btstrMVar conn = do
  putStrLn "Connected!"
  -- Announce strategy to server
  WS.sendTextData conn . A.encode $ Default
  flip finally disconnect $
    forever $ do
      btstr <- WS.receiveData conn
      putMVar btstrMVar btstr
  where
    disconnect = WS.sendClose conn ("Bye!" :: T.Text)

getWSStream :: S.SerialT IO ByteString
getWSStream = do
  btstrMVar <- liftIO newEmptyMVar
  liftIO . forkIO . withSocketsDo $
    WS.runClient "127.0.0.1" 8082 "/" (wsClient btstrMVar)
  S.repeatM . liftIO $ takeMVar btstrMVar

getEventStream :: S.SerialT IO ByteString -> S.SerialT IO Event
getEventStream = S.mapM decodeOrFail
  where
    decodeOrFail :: ByteString -> IO Event
    decodeOrFail btstr =
      maybe (throw $ InvalidEvent btstr) return (A.decode btstr)

-- Game
-------------------------------------------------------------------------------
eventHandler ::
     Int -> SV.Client IO API -> ClientState -> Event -> IO ClientState
eventHandler myId postMove gameState event = do
  print event
  return gameState

-- Main
--------------------------------------------------------------------------------
runClient :: IO ()
runClient = do
  manager' <- newManager defaultManagerSettings
  let postMove = hoistHTTPClient manager'
  maybeDecompStream <- S.uncons getWSStream
  (initialData, streamTail) <-
    maybe (throw NoStreamFound) return maybeDecompStream
  (idAssignment, eventHistory) <-
    maybe (throw $ CannotParse initialData) return (A.decode initialData)
  putStrLn $ formatEventHistory eventHistory
  let (IdAssignment myId) = idAssignment
      eventStream = getEventStream streamTail
  S.foldlM' (eventHandler myId postMove) (eventHistory, "gsp") eventStream
  return ()
