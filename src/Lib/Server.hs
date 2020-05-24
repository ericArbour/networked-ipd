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
    , pidCounter :: PlayerId
    , eventHistory :: [PublicEvent]
    }

-- Helpers
-------------------------------------------------------------------------------------
seconds :: Int -> Int
seconds = (* 1000000)

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
  ServerState {players = [], pidCounter = 1, eventHistory = []}

removePlayer :: Player -> [Player] -> [Player]
removePlayer player = filter ((/= pid player) . pid)

updatePlayerScore :: Int -> Int -> [Player] -> [Player]
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
      removePlayer p ps

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

broadcast :: MVar PublicEvent -> MVar ServerState -> IO ()
broadcast pubEventMVar serverStateMVar = do
  forkIO . forever $ do
    event <- takeMVar pubEventMVar
    serverState <-
      modifyMVar serverStateMVar $ \serverState -> do
        let serverState' =
              ServerState
                { players = players serverState
                , pidCounter = pidCounter serverState
                , eventHistory = event : eventHistory serverState
                }
        return (serverState', serverState')
    let players' = players serverState
    forM_ players' $ \player -> WS.sendTextData (wsConn player) (A.encode event)
  return ()

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
application ::
     MVar ServerState -> MVar ServerEvent -> WS.PendingConnection -> IO ()
application serverStateMVar announcementMVar pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) $
    -- Process initial announcement
   do
    btstr <- WS.receiveData conn
    case A.decode btstr of
      (Just strategy') -> do
        newPlayer <-
          modifyMVar serverStateMVar $ \serverState ->
            let currentPid = pidCounter serverState
                newPlayer =
                  Player
                    { pid = currentPid
                    , score = 0
                    , strategy = strategy'
                    , wsConn = conn
                    }
                players' = newPlayer : players serverState
             in return
                  ( ServerState
                      { players = players'
                      , pidCounter = currentPid + 1
                      , eventHistory = eventHistory serverState
                      }
                  , newPlayer)
        let newPid = IdAssignment (pid newPlayer)
        eventHistory <- eventHistory <$> readMVar serverStateMVar
        WS.sendTextData conn $ A.encode (newPid, eventHistory)
        putMVar announcementMVar $
          Join (pid newPlayer) conn (strategy newPlayer)
        flip finally (disconnect newPlayer) $ keepConnAlive conn
      _ -> WS.sendTextData conn $ T.pack "Invalid strategy."
  where
    disconnect newPlayer = do
      modifyMVar_ serverStateMVar $ \serverState ->
        let players' = removePlayer newPlayer (players serverState)
         in return $
            ServerState
              { players = players'
              , pidCounter = pidCounter serverState
              , eventHistory = eventHistory serverState
              }
      putMVar announcementMVar $ Quit (pid newPlayer)

runWSServer :: Int -> MVar ServerState -> S.SerialT IO ServerEvent
runWSServer port serverStateMVar = do
  announcementMVar <- liftIO newEmptyMVar
  liftIO . forkIO . WS.runServer "127.0.0.1" port $
    application serverStateMVar announcementMVar
  S.repeatM . liftIO $ takeMVar announcementMVar

-- Game
--------------------------------------------------------------------------------------
gameStartEmitter :: MVar (Maybe Game) -> S.SerialT IO ServerEvent
gameStartEmitter gameMVar = do
  startNewGameMVar <- liftIO newEmptyMVar
  liftIO . forkIO . forever $ do
    threadDelay $ seconds 1
    maybeGame <- readMVar gameMVar
    case maybeGame of
      Nothing -> putMVar startNewGameMVar StartNewGame
      Just (Game _ _ _ _) -> return ()
  S.repeatM . liftIO $ takeMVar startNewGameMVar

scoreGame :: Move -> Move -> (Int, Int)
scoreGame p1Move p2Move = (1, 1)

handleServerEvent ::
     MVar ServerState
  -> MVar (Maybe Game)
  -> MVar PublicEvent
  -> ServerEvent
  -> IO ()
handleServerEvent serverStateMVar gameMVar pubEventMVar event = do
  case event of
    Join pid wsConn strat -> putMVar pubEventMVar (PlayerJoin pid strat)
    StartNewGame -> do
      serverState <- readMVar serverStateMVar
      maybeNewGame <-
        modifyMVar gameMVar $ \maybeGame -> do
          case maybeGame of
            Just (Game p1 p2 m1 m2) -> return (maybeGame, Nothing)
            Nothing -> do
              let players' = players serverState
                  playerCount = length players'
              if playerCount < 2
                then return (Nothing, Nothing)
                else do
                  rn1 <- randomRIO (0, playerCount - 1)
                  rn2 <- randomRIO (0, playerCount - 1)
                  let idx1 = rn1
                      idx2 = getUniqueIdx rn1 rn2
                      p1 = players' !! idx1
                      p2 = players' !! idx2
                      newGame = Game (pid p1) (pid p2) Nothing Nothing
                  return (Just newGame, Just newGame)
      case maybeNewGame of
        Just (Game pid1 pid2 _ _) -> putMVar pubEventMVar $ NewGame pid1 pid2
        Nothing -> return ()
    Quit pid -> putMVar pubEventMVar (PlayerQuit pid)
    GameMove pid move -> do
      maybeGame <-
        modifyMVar gameMVar $ \maybeGame ->
          case maybeGame of
            Nothing -> return (Nothing, Nothing)
            Just (Game pid1 pid2 maybeP1Move maybeP2Move)
              | pid == pid1 && isNothing maybeP1Move -> do
                let game' = Just $ Game pid1 pid2 (Just move) maybeP2Move
                return (game', game')
              | pid == pid2 && isNothing maybeP2Move -> do
                let game' = Just $ Game pid1 pid2 maybeP1Move (Just move)
                return (game', game')
              | otherwise -> return (maybeGame, maybeGame)
      case maybeGame of
        Just (Game pid1 pid2 (Just p1Move) (Just p2Move)) -> do
          let (p1Score, p2Score) = scoreGame p1Move p2Move
          serverState <-
            modifyMVar serverStateMVar $ \serverState -> do
              let players' =
                    updatePlayerScore pid2 p2Score .
                    updatePlayerScore pid1 p1Score $
                    players serverState
                  serverState' =
                    ServerState
                      { players = players'
                      , pidCounter = pidCounter serverState
                      , eventHistory = eventHistory serverState
                      }
              return (serverState', serverState')
          modifyMVar_ gameMVar $ \_ -> return Nothing
          putMVar pubEventMVar $ PlayerMove pid1 p1Move
          putMVar pubEventMVar $ PlayerMove pid2 p2Move
          let players' = players serverState
              playerScores = getPlayerScores players'
          putMVar pubEventMVar $ GameResult playerScores
          putStrLn $ formatPlayerScores playerScores
          return ()
        _ -> return ()
  where
    getUniqueIdx rn1 rn2
      | rn1 /= rn2 = rn2
      | rn2 == 0 = 1
      | otherwise = rn2 - 1

-- Main
--------------------------------------------------------------------------------------
runServer :: IO ()
runServer = do
  putStrLn "Starting server..."
  serverStateMVar <- newMVar initialServerState
  gameMVar <- newMVar Nothing
  pubEventMVar <- newEmptyMVar
  let moveStream = runHTTPServer httpPort
      announcementStream = runWSServer wsPort serverStateMVar
      gameStartStream = gameStartEmitter gameMVar
      serverEventStream =
        moveStream `S.parallel` announcementStream `S.parallel` gameStartStream
  putStrLn $ "HTTP server listening on port " <> show httpPort
  putStrLn $ "Websocket server listening on port " <> show wsPort
  broadcast pubEventMVar serverStateMVar
  S.drain .
    S.mapM (handleServerEvent serverStateMVar gameMVar pubEventMVar) .
    S.mapM logServerEvent $
    serverEventStream
  where
    httpPort = 8081
    wsPort = 8082
