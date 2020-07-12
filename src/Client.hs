{-# LANGUAGE OverloadedStrings #-}

module Client
  ( runClient
  , getMove
  , Strategy(..)
  , MoveMap
  , MoveAgainst(..)
  ) where

-- startup args :set args --host "127.0.0.1" --http-port 8081 --ws-port 8082 --strategy=Default
import Control.Concurrent
import Control.Exception (Exception, finally, throw)
import Control.Monad (forever)
import Control.Monad.Trans (liftIO)
import Data.ByteString.Lazy (ByteString)
import Data.List (dropWhile, find, intersperse)
import Data.Maybe (listToMaybe, maybe)
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable)
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.Socket (withSocketsDo)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit
import System.IO
import System.Random (randomRIO)
import Text.Read (readMaybe)

import qualified Data.Aeson as A
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import qualified Servant.Client as SV
import qualified Streamly as S
import qualified Streamly.Prelude as S

import qualified Shared as Move (Move(..))
import Shared (API, IdAssignment(..), PlayerId, PlayerMove(..), PublicEvent(..))

type Host = String

type WsPort = Int

type HttpPort = Int

data Strategy
  = Defect
  | Cooperate
  | Random5050
  | Random8020
  | Random9010
  | TitForTat
  | TitForTwoTats
  | Vigilante
  | ForgivingTitForTat
  | Ostracize
  deriving (Eq, Read, Show)

data MoveAgainst =
  MoveAgainst PlayerId Move.Move
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

hoistHTTPClient :: Host -> HttpPort -> Manager -> SV.Client IO API
hoistHTTPClient host port manager =
  SV.hoistClient api (handleError . getIOClient) (SV.client api)
  where
    baseurl = SV.BaseUrl SV.Http host port ""
    clientEnv :: SV.ClientEnv
    clientEnv = SV.mkClientEnv manager baseurl
    getIOClient :: SV.ClientM a -> IO (Either SV.ClientError a)
    getIOClient = flip SV.runClientM clientEnv
    handleError :: IO (Either SV.ClientError a) -> IO a
    handleError = fmap (either (error . show) id)

postMoves :: Host -> HttpPort -> MVar PlayerMove -> IO ()
postMoves host port myMovesMVar = do
  manager <- newManager defaultManagerSettings
  let postMove = hoistHTTPClient host port manager
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

getWSStream :: Host -> WsPort -> S.SerialT IO ByteString
getWSStream host port = do
  btstrMVar <- liftIO newEmptyMVar
  liftIO . forkIO . withSocketsDo $
    WS.runClient host port "/" (wsClient btstrMVar)
  S.repeatM . liftIO $ takeMVar btstrMVar

decodeOrFail :: ByteString -> IO PublicEvent
decodeOrFail btstr = maybe (throw $ InvalidEvent btstr) return (A.decode btstr)

-- Command Line Args
-------------------------------------------------------------------------------
data Arg
  = Help
  | HostArg String
  | HttpPortArg String
  | WsPortArg String
  | StrategyArg String
  deriving (Eq, Show)

options :: [OptDescr Arg]
options =
  [ Option [] ["help"] (NoArg Help) "Print this help message."
  , Option ['h'] ["host"] (ReqArg HostArg "Host") "The server host location."
  , Option
      ['w']
      ["ws-port"]
      (ReqArg WsPortArg "WS Port")
      "The websocket port on the server."
  , Option
      ['p']
      ["http-port"]
      (ReqArg HttpPortArg "HTTP Port")
      "The http port on the server."
  , Option
      ['s']
      ["strategy"]
      (ReqArg StrategyArg "Strategy")
      ("The strategy the client will use when playing the game.\n" <>
       "Valid strategies are:\n" <>
       "Defect: Always defect.\n" <>
       "Cooperate: Always cooperate.\n" <>
       "Random5050: Randomly cooperate or defect with 50% probability " <>
       "of defecting.\n" <>
       "Random8020: Randomly cooperate or defect with 20% probability " <>
       "of defecting.\n" <>
       "Random9010: Randomly cooperate or defect with 10% probability " <>
       "of defecting.\n" <>
       "TitForTat: If you defected against me last time we played, " <>
       "then I’ll defect\nagainst you this time, " <>
       "and otherwise I’ll cooperate.\n" <>
       "TitForTwoTats: If you defected against me the last two times we " <>
       "played,\nthen I’ll defect against you this time, " <>
       "and otherwise I’ll cooperate.\n" <>
       "Vigilante: If you defected against the last person you played, " <>
       "then I’ll\ndefect against you this time, and otherwise I’ll " <>
       "cooperate.\n" <>
       "ForgivingTitForTat: If you defected against me the last time we " <>
       "played,\nthen I’ll defect against you this time with 50% " <>
       "probability, and otherwise\nI’ll cooperate.\n" <>
       "Ostracize: If you’ve ever defected against anybody, then I’ll " <>
       "always\ndefect against you, and otherwise I’ll always cooperate.")
  ]

processArgs :: IO (Host, WsPort, HttpPort, Strategy)
processArgs = do
  args <- getArgs
  case getOpt Permute options args of
    (args, _, [])
      | Help `elem` args -> do
        putStrLn (usageInfo header options)
        exitSuccess
      | otherwise ->
        case parseArgs args of
          Nothing -> do
            hPutStrLn
              stderr
              ("Please provide the required arguments.\n" <>
               usageInfo header options)
            exitWith (ExitFailure 1)
          Just tup -> return tup
    (_, _, errs) -> do
      hPutStrLn stderr (concat errs <> usageInfo header options)
      exitWith (ExitFailure 1)
  where
    header =
      "Usage: client-exe " <>
      "(--host <HOST> --port <PORT> --strategy <STRATEGY> | --help)"

parseArgs :: [Arg] -> Maybe (Host, WsPort, HttpPort, Strategy)
parseArgs opts = do
  HostArg hostStr <- find isHostArg opts
  WsPortArg wsPortStr <- find isWsPortArg opts
  wsPort <- readMaybe wsPortStr
  HttpPortArg httpPortStr <- find isHttpPortArg opts
  httpPort <- readMaybe httpPortStr
  StrategyArg strategyStr <- find isStrategyArg opts
  strategy <- readMaybe strategyStr
  return (hostStr, wsPort, httpPort, strategy)
  where
    isHostArg (HostArg _) = True
    isHostArg _ = False
    isHttpPortArg (HttpPortArg _) = True
    isHttpPortArg _ = False
    isWsPortArg (WsPortArg _) = True
    isWsPortArg _ = False
    isStrategyArg (StrategyArg _) = True
    isStrategyArg _ = False

-- Game
-------------------------------------------------------------------------------
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

eventHandler ::
     Strategy
  -> PlayerId
  -> MVar PlayerMove
  -> MoveMap
  -> PublicEvent
  -> IO MoveMap
eventHandler strategy myId myMovesMVar moveMap event = do
  print event
  case event of
    NewGame pid1 pid2
      | pid1 == myId || pid2 == myId -> do
        let opId =
              if pid1 == myId
                then pid2
                else pid1
        myMove <- getMove strategy myId opId moveMap
        putMVar myMovesMVar $ PlayerMove myId myMove
        return moveMap
      | otherwise -> return moveMap
    GameResult {} -> return $ insertMoveAgainsts event moveMap
    _ -> return moveMap

getMove :: Strategy -> PlayerId -> PlayerId -> MoveMap -> IO Move.Move
getMove strategy myId opId moveMap =
  case strategy of
    Defect -> return Move.Defect
    Cooperate -> return Move.Cooperate
    Random5050 -> randomDefect 2
    Random8020 -> randomDefect 5
    Random9010 -> randomDefect 10
    TitForTat -> return . reaction $ titForTat myId
    TitForTwoTats -> return . reaction $ titForTwoTats myId
    Vigilante -> return . reaction $ maybe Move.Cooperate opMove . listToMaybe
    ForgivingTitForTat ->
      case reaction (titForTat myId) of
        Move.Defect -> randomDefect 2
        Move.Cooperate -> return Move.Cooperate
    Ostracize -> return . reaction $ maybe Move.Cooperate opMove . find isDefect
  where
    reaction f = maybe Move.Cooperate f $ M.lookup opId moveMap

randomDefect :: Int -> IO Move.Move
randomDefect denom = do
  rn <- randomRIO (1, denom)
  return $
    if rn == 1
      then Move.Defect
      else Move.Cooperate

isDefect :: MoveAgainst -> Bool
isDefect (MoveAgainst _ Move.Defect) = True
isDefect _ = False

isAgainstMe :: PlayerId -> MoveAgainst -> Bool
isAgainstMe myId (MoveAgainst id _) = myId == id

opMove :: MoveAgainst -> Move.Move
opMove (MoveAgainst _ move) = move

titForTat :: PlayerId -> [MoveAgainst] -> Move.Move
titForTat myId = maybe Move.Cooperate opMove . find (isAgainstMe myId)

titForTwoTats :: PlayerId -> [MoveAgainst] -> Move.Move
titForTwoTats myId opMas =
  case dropWhile (not . isAgainstMe myId) opMas of
    [ma] -> Move.Cooperate
    MoveAgainst _ Move.Defect:mas -> titForTat myId mas
    _ -> Move.Cooperate

-- Main
--------------------------------------------------------------------------------
runClient :: IO ()
runClient = do
  (host, wsPort, httpPort, strategy) <- processArgs
  maybeDecompStream <- S.uncons $ getWSStream host wsPort
  (initialData, streamTail) <-
    maybe (throw NoStreamFound) return maybeDecompStream
  (idAssignment, eventHistory) <-
    maybe (throw $ CannotParse initialData) return (A.decode initialData)
  putStrLn $ formatEventHistory eventHistory
  myMovesMVar <- newEmptyMVar
  let (IdAssignment myId) = idAssignment
      moveMap = getInitialMoveMap eventHistory
  print moveMap
  postMoves host httpPort myMovesMVar
  S.foldlM' (eventHandler strategy myId myMovesMVar) moveMap .
    S.mapM decodeOrFail $
    streamTail
  return ()
