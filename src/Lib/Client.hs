{-# LANGUAGE OverloadedStrings #-}

module Lib.Client
  ( runClient
  ) where

import Control.Concurrent
import Control.Exception (Exception, finally, throw)
import Control.Monad (forever)
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans (liftIO)
import Data.Maybe (fromJust, isNothing)
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable)
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.Socket (withSocketsDo)

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
delay :: Int
delay = 1000000

data GameException
  = InvalidStrategy
  | CannotParseId
  deriving (Show, Typeable)

instance Exception GameException

idPrefix :: T.Text
idPrefix = "Your id is: "

parseId :: T.Text -> Maybe Int
parseId = Just . read . T.unpack . T.replace idPrefix ""

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

getEventStream :: S.SerialT (StateT (Maybe Int) IO) T.Text
getEventStream = do
  eventMVar <- liftIO newEmptyMVar
  liftIO .
    forkIO .
    withSocketsDo $ WS.runClient "127.0.0.1" 8082 "/" (wsClient eventMVar)
  S.repeatM . liftIO $ takeMVar eventMVar

-- Game
-------------------------------------------------------------------------------
fakeGameLogic :: SV.Client IO API -> T.Text -> StateT (Maybe Int) IO ()
fakeGameLogic postMove event = do
  liftIO $ T.putStrLn event
  liftIO $ threadDelay delay
  maybeId <- get
  let myId = fromJust maybeId
  liftIO . postMove $ MoveInfo {userId = myId, move = Defect}
  return ()

eventHandler :: SV.Client IO API -> T.Text -> StateT (Maybe Int) IO ()
eventHandler postMove event = do
  maybeId <- get
  case event of
    _
        -- If server does not assign an id after initial announcement, assume invalid strategy
      | isNothing maybeId && not (idPrefix `T.isPrefixOf` event) ->
        throw InvalidStrategy
        -- If server does assign an id after initial announcement, store id on state and proceed to game
      | isNothing maybeId && idPrefix `T.isPrefixOf` event -> do
        put $ parseId event
        fakeGameLogic postMove event
      | otherwise -> fakeGameLogic postMove event

-- Main
--------------------------------------------------------------------------------
runClient :: IO ()
runClient = do
  manager' <- newManager defaultManagerSettings
  let postMove = hoistHTTPClient manager'
      runEventStream =
        S.runStream . S.mapM (eventHandler postMove) $ getEventStream
  evalStateT runEventStream Nothing
