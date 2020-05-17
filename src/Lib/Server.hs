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
  , Event(..)
  , IdAssignment(..)
  , Move(..)
  , PlayerId
  , PlayerMove(..)
  , Strategy(..)
  )

-- TODOS
-- 1. Restructure game / server state, possibly move out of mvar.
-- 2. Do not broadcast move events until games are over.
-- Helpers
-------------------------------------------------------------------------------------
seconds :: Int -> Int
seconds = (* 1000000)

-- HTTP
-------------------------------------------------------------------------------------
server :: MVar PlayerMove -> SV.Server API
server moveMVar = postMove
  where
    postMove :: PlayerMove -> SV.Handler SV.NoContent
    postMove moveInfo = do
      liftIO $ putMVar moveMVar moveInfo
      return SV.NoContent

api :: SV.Proxy API
api = SV.Proxy

app :: MVar PlayerMove -> SV.Application
app moveMVar = SV.serve api (server moveMVar)

-- Does it  make sense to restrict server to PlayerMove type only to map to Move Event?
runHTTPServer :: Int -> S.SerialT IO Event
runHTTPServer port = do
  moveMVar <- liftIO newEmptyMVar
  liftIO . forkIO $ run port (app moveMVar)
  S.map playerMoveToEvent . S.repeatM . liftIO $ takeMVar moveMVar
  where
    playerMoveToEvent (PlayerMove i m) = Move i m

-- WebSockets
--------------------------------------------------------------------------------------
data Player =
  Player
    { pid :: PlayerId
    , score :: Int
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
    , game :: Maybe Game
    , eventHistory :: [Event]
    }

initialServerState :: ServerState
initialServerState =
  ServerState {players = [], pidCounter = 1, game = Nothing, eventHistory = []}

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

formatPlayerScores :: [Player] -> String
formatPlayerScores = foldr (<>) "" . intersperse "\n" . map playerToString
  where
    playerToString p =
      "Player Id: " <> show (pid p) <> ", Strategy: " <> show (strategy p) <>
      ", Score: " <>
      show (score p) <>
      "."

logEvent :: MVar ServerState -> Event -> IO Event
logEvent serverStateMVar event = do
  print event
  modifyMVar_ serverStateMVar $ \serverState ->
    return $
    ServerState
      { players = players serverState
      , pidCounter = pidCounter serverState
      , game = game serverState
      , eventHistory = event : eventHistory serverState
      }
  return event

broadcastEvent :: MVar ServerState -> Event -> IO Event
broadcastEvent serverStateMVar event = do
  serverState <- readMVar serverStateMVar
  let players' = players serverState
  forM_ players' $ \player -> WS.sendTextData (wsConn player) (A.encode event)
  return event

-- Handler for all additional incoming websocket data
-- The server doesn't accept websocket data after initial announcement so it is ignored
keepConnAlive :: WS.Connection -> IO ()
keepConnAlive conn =
  forever $ do
    swallowTextMsg
    return ()
  where
    swallowTextMsg :: IO T.Text
    swallowTextMsg = WS.receiveData conn

-- Handle incoming websocket connection requests
application :: MVar ServerState -> MVar Event -> WS.PendingConnection -> IO ()
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
                      , game = game serverState
                      , eventHistory = eventHistory serverState
                      }
                  , newPlayer)
        let newPid = IdAssignment (pid newPlayer)
        eventHistory <- eventHistory <$> readMVar serverStateMVar
        WS.sendTextData conn $ A.encode (newPid, eventHistory)
        putMVar announcementMVar $ Join (pid newPlayer) (strategy newPlayer)
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
              , game = game serverState
              , eventHistory = eventHistory serverState
              }
      putMVar announcementMVar $ Leave (pid newPlayer)

runWSServer :: Int -> MVar ServerState -> S.SerialT IO Event
runWSServer port serverStateMVar = do
  announcementMVar <- liftIO newEmptyMVar
  liftIO . forkIO . WS.runServer "127.0.0.1" port $
    application serverStateMVar announcementMVar
  S.repeatM . liftIO $ takeMVar announcementMVar

-- Game
--------------------------------------------------------------------------------------
gameMaker :: MVar ServerState -> S.SerialT IO Event
gameMaker serverStateMVar = do
  newGameMVar <- liftIO newEmptyMVar
  liftIO . forkIO . forever $ do
    threadDelay $ seconds 1
    maybeGame <-
      modifyMVar serverStateMVar $ \serverState -> do
        let players' = players serverState
            playerCount = length players'
        if playerCount < 2 || isJust (game serverState)
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
                  , pidCounter = pidCounter serverState
                  , game = Just newGame
                  , eventHistory = eventHistory serverState
                  }
              , Just newGame)
    case maybeGame of
      Just (Game pid1 pid2 _ _) -> putMVar newGameMVar $ GameStart pid1 pid2
      Nothing -> return ()
  S.repeatM . liftIO $ takeMVar newGameMVar
  where
    getUniqueIdx rn1 rn2
      | rn1 /= rn2 = rn2
      | rn2 == 0 = 1
      | otherwise = rn2 - 1

scoreGame :: Move -> Move -> (Int, Int)
scoreGame p1Move p2Move = (1, 1)

updateGame :: MVar ServerState -> Event -> IO ()
updateGame serverStateMVar event =
  case event of
    Join _ _ -> return ()
    GameStart _ _ -> return ()
    Leave _ -> return ()
    Move pid move -> do
      serverState <-
        modifyMVar serverStateMVar $ \serverState ->
          case game serverState of
            Nothing -> return (serverState, serverState)
            Just (Game pid1 pid2 maybeP1Move maybeP2Move)
              | pid == pid1 && isNothing maybeP1Move -> do
                let game' = Game pid1 pid2 (Just move) maybeP2Move
                    serverState' = updateServerStateGame serverState game'
                return (serverState', serverState')
              | pid == pid2 && isNothing maybeP2Move -> do
                let game' = Game pid1 pid2 maybeP1Move (Just move)
                    serverState' = updateServerStateGame serverState game'
                return (serverState', serverState')
              | otherwise -> return (serverState, serverState)
              where updateServerStateGame serverState updatedGame =
                      ServerState
                        { players = players serverState
                        , pidCounter = pidCounter serverState
                        , game = Just updatedGame
                        , eventHistory = eventHistory serverState
                        }
      case game serverState of
        Just (Game pid1 pid2 (Just p1Move) (Just p2Move)) -> do
          let (p1Score, p2Score) = scoreGame p1Move p2Move
          serverState' <-
            modifyMVar serverStateMVar $ \serverState -> do
              let players' =
                    updatePlayerScore pid2 p2Score .
                    updatePlayerScore pid1 p1Score $
                    players serverState
                  serverState' =
                    ServerState
                      { players = players'
                      , pidCounter = pidCounter serverState
                      , game = Nothing
                      , eventHistory = eventHistory serverState
                      }
              return (serverState', serverState')
          putStrLn . formatPlayerScores $ players serverState'
          return ()
        _ -> return ()

-- Main
--------------------------------------------------------------------------------------
runServer :: IO ()
runServer = do
  putStrLn "Starting server..."
  serverStateMVar <- newMVar initialServerState
  let moveStream = runHTTPServer httpPort
      announcementStream = runWSServer wsPort serverStateMVar
      gameStream = gameMaker serverStateMVar
      eventStream =
        moveStream `S.parallel` announcementStream `S.parallel` gameStream
  putStrLn $ "HTTP server listening on port " <> show httpPort
  putStrLn $ "Websocket server listening on port " <> show wsPort
  S.runStream .
    S.mapM (updateGame serverStateMVar) .
    S.mapM (broadcastEvent serverStateMVar) . S.mapM (logEvent serverStateMVar) $
    eventStream
  where
    httpPort = 8081
    wsPort = 8082
