{-# LANGUAGE OverloadedStrings #-}

module Lib.Client
  ( runClient
  ) where

import Control.Concurrent
import Control.Exception (Exception, finally, throw)
import Control.Monad (forever)
import Control.Monad.Trans (liftIO)
import Data.ByteString.Lazy (ByteString)
import Data.List (intersperse)
import Data.Maybe (maybe)
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable)
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.Socket (withSocketsDo)
import Text.Read (readMaybe)

import qualified Data.Aeson as A
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import qualified Servant.Client as SV
import qualified Streamly as S
import qualified Streamly.Prelude as S

import Lib.Shared
  ( API
  , IdAssignment(..)
  , Move(..)
  , MovePost(..)
  , PlayerId
  , PublicEvent(..)
  , Strategy(..)
  )

data MoveAgainst =
  MoveAgainst Move PlayerId

type MoveMap = M.Map PlayerId [MoveAgainst]

data GameException
  = InvalidStrategy
  | NoStreamFound
  | CannotParse ByteString
  | InvalidEvent ByteString
  deriving (Show, Typeable)

instance Exception GameException

-- Helpers
-------------------------------------------------------------------------------
formatEventHistory :: [PublicEvent] -> String
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

movePoster :: MVar MovePost -> IO ()
movePoster movePostMVar = do
  manager' <- newManager defaultManagerSettings
  let postMove = hoistHTTPClient manager'
  forkIO . forever $ takeMVar movePostMVar >>= postMove
  return ()

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

decodeOrFail :: ByteString -> IO PublicEvent
decodeOrFail btstr = maybe (throw $ InvalidEvent btstr) return (A.decode btstr)

-- Game
-------------------------------------------------------------------------------
eventHandler ::
     PlayerId -> MVar MovePost -> MoveMap -> PublicEvent -> IO MoveMap
eventHandler myId movePostMVar moveMap event = do
  print event
  case event of
    NewGame id1 id2 ->
      if id1 == myId || id2 == myId
        then do
          putMVar movePostMVar $ MovePost myId Defect
          return moveMap
        else return moveMap
    _ -> return moveMap

getMoveMap :: [PublicEvent] -> MoveMap
getMoveMap publicEvents = M.empty

-- Main
--------------------------------------------------------------------------------
runClient :: IO ()
runClient = do
  maybeDecompStream <- S.uncons getWSStream
  (initialData, streamTail) <-
    maybe (throw NoStreamFound) return maybeDecompStream
  (idAssignment, eventHistory) <-
    maybe (throw $ CannotParse initialData) return (A.decode initialData)
  putStrLn $ formatEventHistory eventHistory
  movePostMVar <- newEmptyMVar
  let (IdAssignment myId) = idAssignment
      moveMap = getMoveMap eventHistory
  movePoster movePostMVar
  S.foldlM' (eventHandler myId movePostMVar) moveMap . S.mapM decodeOrFail $
    streamTail
  return ()
