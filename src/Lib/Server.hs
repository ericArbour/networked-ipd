{-# LANGUAGE OverloadedStrings #-}

module Lib.Server
  ( runServer
  ) where

import Control.Concurrent
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
  , MovePost(..)
  , PlayerId
  , PublicEvent(..)
  , Score
  , Strategy(..)
  )

-- Types
-- ----------------------------------------------------------------------------------
instance Show WS.Connection where
  show x = "wsConn"

data ServerEvent
  = Join PlayerId WS.Connection Strategy
  | Quit PlayerId
  | GameMove PlayerId Move
  | StartNewGame
  deriving (Show)

data Player =
  Player
    { pid :: PlayerId
    , score :: Score
    , strategy :: Strategy
    , wsConn :: WS.Connection
    }

data Game =
  Game PlayerId PlayerId (Maybe Move) (Maybe Move)
  deriving (Show)

data ServerState =
  ServerState
    { players :: [Player]
    , eventHistory :: [PublicEvent]
    , game :: Maybe Game
    }

-- Helpers
-------------------------------------------------------------------------------------
seconds :: Int -> Int
seconds = (* 1000000)

getUniqueIdx :: Int -> Int -> Int
getUniqueIdx idx1 idx2
  | idx1 /= idx2 = idx2
  | idx2 == 0 = 1
  | otherwise = idx2 - 1

-- HTTP
-------------------------------------------------------------------------------------
server :: MVar MovePost -> SV.Server API
server moveMVar = postMove
  where
    postMove :: MovePost -> SV.Handler SV.NoContent
    postMove movePost = do
      liftIO $ putMVar moveMVar movePost
      return SV.NoContent

api :: SV.Proxy API
api = SV.Proxy

app :: MVar MovePost -> SV.Application
app moveMVar = SV.serve api (server moveMVar)

runHTTPServer :: Int -> S.SerialT IO ServerEvent
runHTTPServer port = do
  moveMVar <- liftIO newEmptyMVar
  liftIO . forkIO $ run port (app moveMVar)
  S.map movePostToEvent . S.repeatM . liftIO $ takeMVar moveMVar
  where
    movePostToEvent (MovePost pid m) = GameMove pid m

-- WebSockets
--------------------------------------------------------------------------------------
initialServerState :: ServerState
initialServerState =
  ServerState {players = [], eventHistory = [], game = Nothing}

removePlayer :: PlayerId -> [Player] -> [Player]
removePlayer pid' = filter ((/= pid') . pid)

updatePlayerScore :: PlayerId -> Int -> [Player] -> [Player]
updatePlayerScore targetPid val ps =
  case find (\p -> (pid p) == targetPid) ps of
    Nothing -> ps
    Just p ->
      Player
        { pid = pid p
        , score = score p + val
        , strategy = strategy p
        , wsConn = wsConn p
        } :
      removePlayer (pid p) ps

getPlayerScores :: [Player] -> [(PlayerId, Score)]
getPlayerScores = map playerToScore
  where
    playerToScore p = (pid p, score p)

formatPlayerScores :: [(PlayerId, Score)] -> String
formatPlayerScores = foldr (<>) "" . intersperse "\n" . map playerScoreToString
  where
    playerScoreToString (pid', score') =
      "Player Id: " <> show pid' <> ", Score: " <> show score' <> "."

logServerEvent :: ServerEvent -> IO ServerEvent
logServerEvent event = do
  putStrLn $ "Server Event: " <> show event
  return event

broadcast :: MVar ServerState -> [PublicEvent] -> IO ()
broadcast serverStateMVar events = do
  forM_ events $ \event ->
    modifyMVar_ serverStateMVar $ \serverState -> do
      let players' = players serverState
          serverState' =
            ServerState
              { players = players'
              , eventHistory = event : eventHistory serverState
              , game = game serverState
              }
      forM_ players' $ \player ->
        WS.sendTextData (wsConn player) (A.encode event)
      return serverState'

-- Handler for all additional incoming websocket data
-- The server doesn't accept websocket data after initial announcement so it is ignored
keepConnAlive :: WS.Connection -> IO ()
keepConnAlive conn =
  forever $ do
    swallowMsg
    return ()
  where
    swallowMsg :: IO T.Text
    swallowMsg = WS.receiveData conn

-- Handle incoming websocket connection requests
application :: MVar Int -> MVar ServerEvent -> WS.PendingConnection -> IO ()
application uuidMVar announcementMVar pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) $
    -- Process initial announcement
   do
    btstr <- WS.receiveData conn
    case A.decode btstr of
      (Just strategy') -> do
        pid' <- modifyMVar uuidMVar $ \uuid -> return (uuid + 1, uuid + 1)
        putMVar announcementMVar $ Join pid' conn strategy'
        flip finally (disconnect pid') $ keepConnAlive conn
      _ -> WS.sendTextData conn $ T.pack "Invalid strategy."
  where
    disconnect pid' = putMVar announcementMVar $ Quit pid'

runWSServer :: Int -> S.SerialT IO ServerEvent
runWSServer port = do
  announcementMVar <- liftIO newEmptyMVar
  uuidMVar <- liftIO $ newMVar 0
  liftIO . forkIO . WS.runServer "127.0.0.1" port $
    application uuidMVar announcementMVar
  S.repeatM . liftIO $ takeMVar announcementMVar

-- Game
--------------------------------------------------------------------------------------
gameStartStream :: S.SerialT IO ServerEvent
gameStartStream = do
  startNewGameMVar <- liftIO newEmptyMVar
  liftIO . forkIO . forever $ do
    threadDelay $ seconds 1
    putMVar startNewGameMVar StartNewGame
  S.repeatM . liftIO $ takeMVar startNewGameMVar

scoreGame :: Move -> Move -> (Int, Int)
scoreGame p1Move p2Move = (1, 1)

handleServerEvent :: MVar ServerState -> ServerEvent -> IO [PublicEvent]
handleServerEvent serverStateMVar event = do
  case event of
    Join pid' wsConn' strategy' -> do
      modifyMVar_ serverStateMVar $ \serverState -> do
        let newPlayer =
              Player
                {pid = pid', wsConn = wsConn', strategy = strategy', score = 0}
            players' = newPlayer : players serverState
            eventHistory' = eventHistory serverState
        WS.sendTextData wsConn' $ A.encode (IdAssignment pid', eventHistory')
        return $
          ServerState
            { players = players'
            , eventHistory = eventHistory serverState
            , game = game serverState
            }
      return [PlayerJoin pid' strategy']
    Quit pid' -> do
      modifyMVar_ serverStateMVar $ \serverState ->
        let players' = removePlayer pid' (players serverState)
         in return $
            ServerState
              { players = players'
              , eventHistory = eventHistory serverState
              -- Todo: Null out game player quits while in game prior to move
              , game = game serverState
              }
      return [PlayerQuit pid']
    StartNewGame -> do
      maybeNewGame <-
        modifyMVar serverStateMVar $ \serverState -> do
          let maybeGame = game serverState
          case maybeGame
            -- Todo: Disconnect players who didn't move in time and start new game
                of
            Just (Game p1 p2 m1 m2) -> return (serverState, Nothing)
            Nothing -> do
              let players' = players serverState
                  playerCount = length players'
              if playerCount < 2
                then return (serverState, Nothing)
                else do
                  rn1 <- randomRIO (0, playerCount - 1)
                  rn2 <- randomRIO (0, playerCount - 1)
                  let idx1 = rn1
                      idx2 = getUniqueIdx rn1 rn2
                      p1 = players' !! idx1
                      p2 = players' !! idx2
                      newGame = Game (pid p1) (pid p2) Nothing Nothing
                  return
                    ( ServerState
                        { players = players serverState
                        , eventHistory = eventHistory serverState
                        , game = Just newGame
                        }
                    , Just newGame)
      case maybeNewGame of
        Just (Game pid1 pid2 _ _) -> return [NewGame pid1 pid2]
        Nothing -> return []
    GameMove pid move -> do
      maybeGame <-
        modifyMVar serverStateMVar $ \serverState -> do
          let maybeGame = game serverState
          case maybeGame of
            Nothing -> return (serverState, Nothing)
            Just (Game pid1 pid2 maybeP1Move maybeP2Move)
              | pid == pid1 && isNothing maybeP1Move -> do
                let game' = Just $ Game pid1 pid2 (Just move) maybeP2Move
                return
                  ( ServerState
                      { players = players serverState
                      , eventHistory = eventHistory serverState
                      , game = game'
                      }
                  , game')
              | pid == pid2 && isNothing maybeP2Move -> do
                let game' = Just $ Game pid1 pid2 maybeP1Move (Just move)
                return
                  ( ServerState
                      { players = players serverState
                      , eventHistory = eventHistory serverState
                      , game = game'
                      }
                  , game')
              | otherwise -> return (serverState, game serverState)
      case maybeGame of
        Just (Game pid1 pid2 (Just p1Move) (Just p2Move)) -> do
          let (p1Score, p2Score) = scoreGame p1Move p2Move
          players' <-
            modifyMVar serverStateMVar $ \serverState -> do
              let players' =
                    updatePlayerScore pid2 p2Score .
                    updatePlayerScore pid1 p1Score $
                    players serverState
                  serverState' =
                    ServerState
                      { players = players'
                      , eventHistory = eventHistory serverState
                      , game = Nothing
                      }
              return (serverState', players')
          let playerScores = getPlayerScores players'
          putStrLn $ formatPlayerScores playerScores
          return
            [ PlayerMove pid1 p1Move
            , PlayerMove pid2 p2Move
            , GameResult playerScores
            ]
        _ -> return []

-- Main
--------------------------------------------------------------------------------------
runServer :: IO ()
runServer = do
  putStrLn "Starting server..."
  serverStateMVar <- newMVar initialServerState
  let moveStream = runHTTPServer httpPort
      announcementStream = runWSServer wsPort
      serverEventStream =
        moveStream `S.parallel` announcementStream `S.parallel` gameStartStream
  putStrLn $ "HTTP server listening on port " <> show httpPort
  putStrLn $ "Websocket server listening on port " <> show wsPort
  S.drain .
    S.mapM (broadcast serverStateMVar) .
    S.mapM (handleServerEvent serverStateMVar) . S.mapM logServerEvent $
    serverEventStream
  where
    httpPort = 8081
    wsPort = 8082
