{-# LANGUAGE OverloadedStrings #-}

module Lib.Server
  ( runServer
  ) where

-- Todo: spin up clients at start up based on configuration
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Monad.Trans (liftIO)
import Data.List (find, intersperse)
import Data.Maybe (isJust, isNothing)
import Network.Wai.Handler.Warp (run)
import System.Random (randomRIO)

import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import qualified Servant as SV
import qualified Streamly as S
import qualified Streamly.Prelude as S

import Lib.Shared
  ( API
  , IdAssignment(..)
  , Move(..)
  , PlayerId
  , PlayerMove(..)
  , PublicEvent(..)
  , Score
  )

-- Types
-- ----------------------------------------------------------------------------------
instance Show WS.Connection where
  show = const "wsConn"

data ServerEvent
  = Join PlayerId WS.Connection
  | Quit PlayerId
  | GameMove PlayerId Move
  | StartNewGame
  deriving (Show)

data Player =
  Player
    { pid :: PlayerId
    , wsConn :: WS.Connection
    , score :: Score
    }

data Game =
  Game PlayerId (Maybe Move) PlayerId (Maybe Move)
  deriving (Show)

data ServerState =
  ServerState
    { players :: [Player]
    , eventHistory :: [PublicEvent]
    , game :: Maybe Game
    }

-- Variables
-------------------------------------------------------------------------------------
-- Todo: move these to configuration file
scoreA :: Int
scoreA = 2

scoreB :: Int
scoreB = 1

scoreC :: Int
scoreC = -1

minScore :: Int
minScore = -200

gameDuration :: Int
gameDuration = seconds 1
  where
    seconds = (* 1000000)

-- HTTP
-------------------------------------------------------------------------------------
server :: MVar PlayerMove -> SV.Server API
server moveMVar = handlePostedMove
  where
    handlePostedMove :: PlayerMove -> SV.Handler SV.NoContent
    handlePostedMove playerMove = do
      liftIO $ putMVar moveMVar playerMove
      return SV.NoContent

api :: SV.Proxy API
api = SV.Proxy

app :: MVar PlayerMove -> SV.Application
app moveMVar = SV.serve api (server moveMVar)

runHTTPServer :: Int -> S.SerialT IO ServerEvent
runHTTPServer port = do
  moveMVar <- liftIO newEmptyMVar
  liftIO . forkIO $ run port (app moveMVar)
  S.map playerMoveToEvent . S.repeatM . liftIO $ takeMVar moveMVar
  where
    playerMoveToEvent (PlayerMove pid m) = GameMove pid m

-- WebSockets
--------------------------------------------------------------------------------------
-- Handle incoming websocket connection requests
application :: TVar Int -> MVar ServerEvent -> WS.PendingConnection -> IO ()
application pidCounterTVar announcementMVar pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) $
    -- Process initial announcement
   do
    pid' <- atomically $ incrementCounter pidCounterTVar
    putMVar announcementMVar $ Join pid' conn
    flip finally (disconnect pid') $ keepConnAlive conn
  where
    incrementCounter pidCounterTVar = do
      nextPid <- readTVar pidCounterTVar >>= \prevId -> return (prevId + 1)
      writeTVar pidCounterTVar nextPid
      return nextPid
    disconnect pid' = putMVar announcementMVar $ Quit pid'
    -- Ignore all additional websocket messages from client
    keepConnAlive conn =
      forever $ do
        WS.receiveData conn :: IO T.Text
        return ()

runWSServer :: Int -> S.SerialT IO ServerEvent
runWSServer port = do
  announcementMVar <- liftIO newEmptyMVar
  pidCounterTVar <- liftIO $ newTVarIO 0
  liftIO . forkIO . WS.runServer "127.0.0.1" port $
    application pidCounterTVar announcementMVar
  S.repeatM . liftIO $ takeMVar announcementMVar

broadcast :: MVar ([Player], PublicEvent) -> IO ()
broadcast broadcastMVar = do
  forkIO . forever $ do
    (players', publicEvent) <- takeMVar broadcastMVar
    forM_ players' $ \player ->
      WS.sendTextData (wsConn player) (A.encode publicEvent)
  return ()

-- Game
--------------------------------------------------------------------------------------
initialServerState :: ServerState
initialServerState =
  ServerState {players = [], eventHistory = [], game = Nothing}

removePlayer :: PlayerId -> [Player] -> [Player]
removePlayer pid' = filter ((/= pid') . pid)

getPlayer :: PlayerId -> [Player] -> Maybe Player
getPlayer targetPid = find (\p -> pid p == targetPid)

updatePlayerScore :: PlayerId -> Int -> [Player] -> [Player]
updatePlayerScore targetPid val ps =
  case getPlayer targetPid ps of
    Nothing -> ps
    Just p ->
      Player {pid = pid p, score = score p + val, wsConn = wsConn p} :
      removePlayer (pid p) ps

getPlayerScores :: [Player] -> [(PlayerId, Score)]
getPlayerScores = map playerToScore
  where
    playerToScore p = (pid p, score p)

scoreGame :: Move -> Move -> (Int, Int)
scoreGame Defect Cooperate = (scoreA, scoreC)
scoreGame Cooperate Defect = (scoreC, scoreA)
scoreGame Cooperate Cooperate = (scoreB, scoreB)
scoreGame Defect Defect = (scoreC, scoreC)

formatPlayerScores :: [(PlayerId, Score)] -> String
formatPlayerScores = foldr (<>) "" . intersperse "\n" . map playerScoreToString
  where
    playerScoreToString (pid', score') =
      "Player Id: " <> show pid' <> ", Score: " <> show score' <> "."

logServerEvent :: ServerEvent -> IO ServerEvent
logServerEvent event = do
  putStrLn $ "Server Event: " <> show event
  return event

gameStartStream :: S.SerialT IO ServerEvent
gameStartStream = do
  startNewGameMVar <- liftIO newEmptyMVar
  liftIO . forkIO . forever $ do
    threadDelay gameDuration
    putMVar startNewGameMVar StartNewGame
  S.repeatM . liftIO $ takeMVar startNewGameMVar

handleStartNewGame ::
     MVar ([Player], PublicEvent) -> ServerState -> IO ServerState
handleStartNewGame broadcastMVar serverState = do
  let maybeGame = game serverState
      players' = players serverState
  (maybeNewGame, updatedPlayers) <-
    case maybeGame of
      Just (Game pid1 m1 pid2 m2)
    -- Handle an incomplete game by kicking players who didn't respond in time
       -> do
        players'' <-
          if isNothing m1
            then kickPlayer pid1 players'
            else return players'
        players''' <-
          if isNothing m2
            then kickPlayer pid2 players''
            else return players''
        maybeGame <- makeNewGame players'''
        return (maybeGame, players''')
      Nothing -> do
        maybeGame <- makeNewGame players'
        return (maybeGame, players')
  case maybeNewGame of
    Just (Game pid1 _ pid2 _) -> do
      let newGameEvent = NewGame pid1 pid2
      putMVar broadcastMVar (updatedPlayers, newGameEvent)
      return $
        ServerState
          { players = updatedPlayers
          , eventHistory = newGameEvent : eventHistory serverState
          , game = maybeNewGame
          }
    Nothing ->
      return $
      ServerState
        { players = updatedPlayers
        , eventHistory = eventHistory serverState
        , game = maybeNewGame
        }
  where
    makeNewGame players' = do
      let playerCount = length players'
      if playerCount < 2
        then return Nothing
        else do
          rn1 <- randomRIO (0, playerCount - 1)
          rn2 <- randomRIO (0, playerCount - 2)
          let idx1 = rn1
              idx2 = getUniqueIdx rn1 rn2
              p1 = players' !! idx1
              p2 = players' !! idx2
          return $ Just $ Game (pid p1) Nothing (pid p2) Nothing
    getUniqueIdx idx1 idx2 =
      if idx2 >= idx1
        then idx2 + 1
        else idx2
    kickPlayer pid' players' =
      case getPlayer pid' players' of
        (Just player) -> do
          WS.sendClose (wsConn player) $
            T.pack "You took too long to make a move, goodbye!"
          return $ removePlayer pid' players'
        Nothing -> return players'

handleServerEvent ::
     MVar ([Player], PublicEvent)
  -> ServerState
  -> ServerEvent
  -> IO ServerState
handleServerEvent broadcastMVar serverState event =
  case event of
    Join pid' wsConn' -> do
      let newPlayer = Player {pid = pid', wsConn = wsConn', score = 0}
          players' = newPlayer : players serverState
          eventHistory' = eventHistory serverState
          joinEvent = PlayerJoin pid'
      WS.sendTextData wsConn' $ A.encode (IdAssignment pid', eventHistory')
      putMVar broadcastMVar (players', joinEvent)
      return $
        ServerState
          { players = players'
          , eventHistory = joinEvent : eventHistory'
          , game = game serverState
          }
    Quit pid' ->
      let players' = removePlayer pid' (players serverState)
          maybeGame = game serverState
          game' =
            case maybeGame of
              Nothing -> maybeGame
              -- Remove game if quitting player was in it
              Just (Game pid1 _ pid2 _) ->
                if pid' == pid1 || pid' == pid2
                  then Nothing
                  else maybeGame
       in return $
          ServerState
            { players = players'
            , eventHistory = eventHistory serverState
            , game = game'
            }
    StartNewGame -> handleStartNewGame broadcastMVar serverState
    GameMove pid move -> do
      let maybeGame = game serverState
          maybeUpdatedGame =
            case maybeGame of
              Nothing -> maybeGame
              Just (Game pid1 maybeP1Move pid2 maybeP2Move)
                | pid == pid1 && isNothing maybeP1Move ->
                  Just $ Game pid1 (Just move) pid2 maybeP2Move
                | pid == pid2 && isNothing maybeP2Move ->
                  Just $ Game pid1 maybeP1Move pid2 (Just move)
                | otherwise -> maybeGame
      case maybeUpdatedGame of
        Just (Game pid1 (Just p1Move) pid2 (Just p2Move)) -> do
          let (p1Score, p2Score) = scoreGame p1Move p2Move
              players' =
                updatePlayerScore pid2 p2Score . updatePlayerScore pid1 p1Score $
                players serverState
              playerScores = getPlayerScores players'
              gameResultEvent = GameResult pid1 p1Move pid2 p2Move
          -- Todo: Kick players whose scores fall too low
          putStrLn "Player Scores:"
          putStrLn "--------------"
          putStrLn $ formatPlayerScores playerScores
          putStrLn "--------------"
          putMVar broadcastMVar (players', gameResultEvent)
          return $
            ServerState
              { players = players'
              , eventHistory = gameResultEvent : eventHistory serverState
              , game = Nothing
              }
        _ ->
          return $
          ServerState
            { players = players serverState
            , eventHistory = eventHistory serverState
            , game = maybeUpdatedGame
            }

-- Main
--------------------------------------------------------------------------------------
runServer :: IO ()
runServer = do
  putStrLn "Starting server..."
  broadcastMVar <- newEmptyMVar
  let moveStream = runHTTPServer httpPort
      announcementStream = runWSServer wsPort
      serverEventStream =
        moveStream `S.parallel` announcementStream `S.parallel` gameStartStream
  putStrLn $ "HTTP server listening on port " <> show httpPort
  putStrLn $ "Websocket server listening on port " <> show wsPort
  broadcast broadcastMVar
  S.foldlM' (handleServerEvent broadcastMVar) initialServerState .
    S.mapM logServerEvent $
    serverEventStream
  return ()
  where
    httpPort = 8081
    wsPort = 8082
