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
  , PlayerId
  , PlayerMove(..)
  , PublicEvent(..)
  , Strategy(..)
  )

data MoveAgainst =
  MoveAgainst PlayerId Move
  deriving (Show)

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

postMoves :: MVar PlayerMove -> IO ()
postMoves myMovesMVar = do
  manager' <- newManager defaultManagerSettings
  let postMove = hoistHTTPClient manager'
  forkIO . forever $ takeMVar myMovesMVar >>= postMove
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
     PlayerId -> MVar PlayerMove -> MoveMap -> PublicEvent -> IO MoveMap
eventHandler myId myMovesMVar moveMap event = do
  print event
  case event of
    NewGame id1 id2 ->
      if id1 == myId || id2 == myId
        then do
          putMVar myMovesMVar $ PlayerMove myId Defect
          print moveMap
          return moveMap
        else return moveMap
    GameResult _ _ _ _ -> return $ insertGameResult event moveMap
    _ -> return moveMap

getInitialMoveMap :: [PublicEvent] -> MoveMap
getInitialMoveMap = foldr insertGameResult M.empty

insertGameResult :: PublicEvent -> MoveMap -> MoveMap
insertGameResult pe moveMap =
  case pe of
    GameResult pid1 p1move pid2 p2move ->
      insertMoveAgainst pid2 pid1 p2move $
      insertMoveAgainst pid1 pid2 p1move moveMap
    _ -> moveMap
  where
    insertMoveAgainst movePid againstPid move moveMap =
      case M.lookup movePid moveMap of
        Nothing -> M.insert movePid [MoveAgainst againstPid move] moveMap
        Just moveAgainsts ->
          M.insert
            movePid
            ((MoveAgainst againstPid move) : moveAgainsts)
            moveMap

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
  myMovesMVar <- newEmptyMVar
  let (IdAssignment myId) = idAssignment
      moveMap = getInitialMoveMap eventHistory
  print moveMap
  postMoves myMovesMVar
  S.foldlM' (eventHandler myId myMovesMVar) moveMap . S.mapM decodeOrFail $
    streamTail
  return ()
