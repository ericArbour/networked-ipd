{-# LANGUAGE OverloadedStrings #-}

module Lib.Server
  ( runServer
  ) where

import Control.Concurrent
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Monad.Trans (liftIO)
import Data.Maybe (isJust)
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
  , PlayerId
  , PlayerMove(..)
  , Strategy(..)
  )

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

runHTTPServer :: Int -> S.SerialT IO Event
runHTTPServer port = do
  moveMVar <- liftIO newEmptyMVar
  liftIO . forkIO $ run port (app moveMVar)
  S.map playerMoveToEvent . S.repeatM . liftIO $ takeMVar moveMVar
  where
    playerMoveToEvent (PlayerMove i m) = MoveEvent i m

-- WebSockets
--------------------------------------------------------------------------------------
data Player =
  Player
    { pid :: PlayerId
    , strategy :: Strategy
    , wsConn :: WS.Connection
    }

data Game =
  Game PlayerId PlayerId
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

broadcastEvent :: MVar ServerState -> Event -> IO ()
broadcastEvent serverStateMVar event = do
  serverState <- readMVar serverStateMVar
  let players' = players serverState
  putStrLn $ show event
  modifyMVar_ serverStateMVar $ \serverState -> do
    return $
      ServerState
        { players = players serverState
        , pidCounter = pidCounter serverState
        , game = game serverState
        , eventHistory = event : eventHistory serverState
        }
  forM_ players' $ \player -> WS.sendTextData (wsConn player) (A.encode event)

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
                  Player {pid = currentPid, strategy = strategy', wsConn = conn}
                players' = newPlayer : (players serverState)
             in return $
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
        putMVar announcementMVar $
          JoinEvent (pid newPlayer) (strategy newPlayer)
        flip finally (disconnect newPlayer) $ keepConnAlive conn
      otherwise -> WS.sendTextData conn $ T.pack "Invalid strategy."
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
      putMVar announcementMVar $ LeaveEvent (pid newPlayer)

runWSServer :: Int -> MVar ServerState -> S.SerialT IO Event
runWSServer port serverStateMVar = do
  announcementMVar <- liftIO newEmptyMVar
  liftIO . forkIO . WS.runServer "127.0.0.1" port $
    application serverStateMVar announcementMVar
  S.repeatM . liftIO $ takeMVar announcementMVar

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
                newGame = Game (pid p1) (pid p2)
            return $
              ( ServerState
                  { players = players serverState
                  , pidCounter = pidCounter serverState
                  , game = Just newGame
                  , eventHistory = eventHistory serverState
                  }
              , Just newGame)
    case maybeGame of
      Just (Game pid1 pid2) -> putMVar newGameMVar $ GameStartEvent pid1 pid2
      Nothing -> return ()
  S.repeatM . liftIO $ takeMVar newGameMVar
  where
    getUniqueIdx rn1 rn2 =
      if rn1 /= rn2
        then rn2
        else if rn2 == 0
               then 1
               else rn2 - 1

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
  putStrLn $ "HTTP server listening on port " <> (show httpPort)
  putStrLn $ "Websocket server listening on port " <> (show wsPort)
  S.runStream $ S.mapM (broadcastEvent serverStateMVar) eventStream
  where
    httpPort = 8081
    wsPort = 8082
