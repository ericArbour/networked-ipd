{-# LANGUAGE OverloadedStrings #-}

module Lib.Server (runServer) where

import Control.Concurrent
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Monad.Trans (liftIO)
import Network.Wai.Handler.Warp (run)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import qualified Servant as SV
import qualified Streamly as S
import qualified Streamly.Prelude as S

import Lib.Shared

-- HTTP
-------------------------------------------------------------------------------------
server :: MVar MoveInfo -> SV.Server API
server moveMVar = postMove
  where
    postMove :: MoveInfo -> SV.Handler SV.NoContent
    postMove moveInfo = do
      liftIO $ putMVar moveMVar moveInfo
      return SV.NoContent

api :: SV.Proxy API
api = SV.Proxy

app :: MVar MoveInfo -> SV.Application
app moveMVar = SV.serve api (server moveMVar)

runHTTPServer :: Int -> S.SerialT IO T.Text
runHTTPServer port = do
  moveMVar <- liftIO newEmptyMVar
  liftIO $ forkIO $ run port (app moveMVar)
  S.map (T.pack . show) $ S.repeatM $ liftIO $ takeMVar moveMVar

-- WebSockets
--------------------------------------------------------------------------------------
data Client =
  Client
    { uid :: Int
    , strategy :: T.Text
    , wsConn :: WS.Connection
    }

data ServerState =
  ServerState
    { clients :: [Client]
    , uidCounter :: Int
    , eventHistory :: T.Text
    }

initialServerState :: ServerState
initialServerState =
  ServerState {clients = [], uidCounter = 1, eventHistory = ""}

numClients :: [Client] -> Int
numClients = length

addClient :: Client -> [Client] -> [Client]
addClient client clients = client : clients

removeClient :: Client -> [Client] -> [Client]
removeClient client = filter ((/= uid client) . uid)

broadcastEvent :: MVar ServerState -> T.Text -> IO ()
broadcastEvent serverStateMVar message = do
  serverState <- readMVar serverStateMVar
  let clients' = clients serverState
  T.putStrLn message
  modifyMVar_ serverStateMVar $ \serverState -> do
    return $
      ServerState
        { clients = clients serverState
        , uidCounter = uidCounter serverState
        , eventHistory = eventHistory serverState <> "\n" <> message
        }
  forM_ clients' $ \client -> WS.sendTextData (wsConn client) message

joinAnnouncement :: Client -> T.Text
joinAnnouncement client =
                "Player #" <> (T.pack $ show $ uid client) <>
                " joined with the strategy " <>
                strategy client <> "."

disconnectAnnouncement :: Client -> T.Text
disconnectAnnouncement client = "Player #" <> (T.pack $ show $ uid client) <> " disconnected."

application :: MVar ServerState -> MVar T.Text -> WS.PendingConnection -> IO ()
application serverStateMVar announcementMVar pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) $ do
    msg <- WS.receiveData conn
    serverState <- readMVar serverStateMVar
    let currentUid = uidCounter serverState
        newClient = Client {uid = currentUid, strategy = msg, wsConn = conn}
    case msg of
      _
        | not (msg == "Default Strategy") ->
          WS.sendTextData conn ("Invalid strategy." :: T.Text)
        | otherwise ->
          flip finally disconnect $ do
            modifyMVar_ serverStateMVar $ \serverState -> do
              let clients' = addClient newClient $ clients serverState
              WS.sendTextData conn $
                T.pack $ "Your id is: " <> (show currentUid)
              WS.sendTextData conn $ (T.pack "Event history: ") <> eventHistory serverState
              putMVar announcementMVar $ joinAnnouncement newClient
              return $
                ServerState
                  { clients = clients'
                  , uidCounter = currentUid + 1
                  , eventHistory = eventHistory serverState
                  }
            keepConnAlive newClient
        where disconnect = do
                modifyMVar_ serverStateMVar $ \serverState ->
                  let clients' = removeClient newClient $ clients serverState
                      currentUid = uidCounter serverState
                   in return $
                      ServerState
                        { clients = clients'
                        , uidCounter = currentUid
                        , eventHistory = eventHistory serverState
                        }
                putMVar announcementMVar $ disconnectAnnouncement newClient 

keepConnAlive :: Client -> IO ()
keepConnAlive client =
  forever $ do
    swallowTextMsg
    return ()
  where
    swallowTextMsg :: IO T.Text
    swallowTextMsg = WS.receiveData $ wsConn client

runWSServer :: Int -> MVar ServerState -> S.SerialT IO T.Text
runWSServer port serverStateMVar = do
  announcementMVar <- liftIO newEmptyMVar
  liftIO $
    forkIO $
    WS.runServer "127.0.0.1" port $ application serverStateMVar announcementMVar
  S.repeatM $ liftIO $ takeMVar announcementMVar

-- Main
--------------------------------------------------------------------------------------
runServer :: IO ()
runServer = do
  putStrLn "Starting server..."
  serverStateMVar <- newMVar initialServerState
  let moveStream = runHTTPServer httpPort
      announcementStream = runWSServer wsPort serverStateMVar
      eventStream = moveStream `S.parallel` announcementStream
  putStrLn $ "HTTP server listening on port " <> (show httpPort)
  putStrLn $ "Websocket server listening on port " <> (show wsPort)
  S.runStream $ S.mapM (broadcastEvent serverStateMVar) eventStream
  where httpPort = 8081
        wsPort = 8082
        
