{-# LANGUAGE OverloadedStrings #-}

module Lib.Client
  ( runClient
  ) where

import Control.Concurrent
import Control.Exception (Exception, finally, throw)
import Control.Monad (forever)
import Control.Monad.Trans (liftIO)
import Data.ByteString.Lazy (ByteString)
import Data.List (find, intersperse)
import Data.Maybe (maybe)
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable)
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.Socket (withSocketsDo)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit
import System.IO
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
  )

data Strategy =
  Default
  deriving (Eq, Read, Show)

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

-- Command Line Args
-------------------------------------------------------------------------------
data Flag
  = StrategyArg String
  | Help
  deriving (Eq, Show)

options :: [OptDescr Flag]
options =
  [ Option
      ['s']
      ["strategy"]
      (ReqArg StrategyArg "Strategy")
      ("The strategy the client will use when playing the game.\n" <>
       "Valid strategies are:\n" <>
       "Defect\n" <>
       "Cooperate\n" <>
       "Random5050\n" <>
       "Random8020\n" <>
       "Random9010\n" <>
       "TitForTat\n" <>
       "TitForTwoTats\n" <>
       "Vigilante\n" <>
       "ForgivingTitForTat\n" <>
       "Ostracise")
  , Option [] ["help"] (NoArg Help) "Print this help message."
  ]

processArgs :: IO Strategy
processArgs = do
  args <- getArgs
  case getOpt Permute options args of
    (opts, _, [])
      | (Help `elem` opts) -> do
        hPutStrLn stdout (usageInfo header options)
        exitWith ExitSuccess
      | not (any isStrategyArg opts) -> do
        hPutStrLn
          stderr
          ("Please provide a strategy.\n" <> usageInfo header options)
        exitWith (ExitFailure 1)
      | otherwise -> return Default
    (_, _, errs) -> do
      hPutStrLn stderr (concat errs <> usageInfo header options)
      exitWith (ExitFailure 1)
  where
    header = "Usage: client-exe [OPTION...]"
    isStrategyArg (StrategyArg _) = True
    isStrategyArg _ = False

-- Game
-------------------------------------------------------------------------------
eventHandler ::
     PlayerId -> MVar PlayerMove -> MoveMap -> PublicEvent -> IO MoveMap
eventHandler myId myMovesMVar moveMap event = do
  print event
  case event of
    NewGame pid1 pid2
      | pid1 == myId -> do
        let myMove = getMove moveMap pid2
        putMVar myMovesMVar $ PlayerMove myId myMove
        return moveMap
      | pid2 == myId -> do
        let myMove = getMove moveMap pid1
        putMVar myMovesMVar $ PlayerMove myId myMove
        return moveMap
      | otherwise -> return moveMap
    GameResult {} -> return $ insertMoveAgainsts event moveMap
    _ -> return moveMap

getInitialMoveMap :: [PublicEvent] -> MoveMap
getInitialMoveMap = foldr insertMoveAgainsts M.empty

insertMoveAgainsts :: PublicEvent -> MoveMap -> MoveMap
insertMoveAgainsts pe moveMap =
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
          M.insert movePid (MoveAgainst againstPid move : moveAgainsts) moveMap

getMove :: MoveMap -> PlayerId -> Move
-- Todo: implement strategies
getMove moveMap opponent = Defect

-- Main
--------------------------------------------------------------------------------
runClient :: IO ()
runClient = do
  processArgs
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
