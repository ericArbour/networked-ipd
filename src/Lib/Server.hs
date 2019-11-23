{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Lib.Server where

import Control.Concurrent
import Control.Exception (finally)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.Char (isPunctuation, isSpace)
import Data.Char (isPunctuation, isSpace)
import Data.List
import Data.Maybe
import Data.String.Conversions
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Calendar
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Network.WebSockets as WS
import Prelude ()
import Prelude.Compat
import Servant
import Servant.Types.SourceT (source)
import Streamly
import qualified Streamly.Prelude as S
import System.Directory
import Text.Blaze
import qualified Text.Blaze.Html
import Text.Blaze.Html.Renderer.Utf8

import Lib.Shared

-- HTTP
server :: MVar MoveInfo -> Server API
server moveMVar = postMove
  where
    postMove :: MoveInfo -> Handler NoContent
    postMove moveInfo = do
      liftIO $ putMVar moveMVar moveInfo
      return NoContent

api :: Proxy API
api = Proxy

app :: MVar MoveInfo -> Application
app moveMVar = serve api (server moveMVar)

runHTTPServer :: SerialT IO T.Text
runHTTPServer = do
  moveMVar <- liftIO newEmptyMVar
  liftIO $ forkIO $ run 8081 (app moveMVar)
  S.map (T.pack . show) $ S.repeatM $ liftIO $ takeMVar moveMVar

-- WebSockets
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
              putMVar announcementMVar $ formatAnnouncement newClient
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
                putMVar announcementMVar $
                  (T.pack $ show $ uid newClient) <> " disconnected"
              formatAnnouncement client =
                "Player #" <> (T.pack $ show $ uid client) <>
                " joined with the strategy " <>
                strategy client <> "."

keepConnAlive :: Client -> IO ()
keepConnAlive client =
  forever $ do
    swallowTextMsg
    return ()
  where
    swallowTextMsg :: IO T.Text
    swallowTextMsg = WS.receiveData $ wsConn client

runWSServer :: MVar ServerState -> SerialT IO T.Text
runWSServer serverStateMVar = do
  announcementMVar <- liftIO newEmptyMVar
  liftIO $
    forkIO $
    WS.runServer "127.0.0.1" 8082 $ application serverStateMVar announcementMVar
  S.repeatM $ liftIO $ takeMVar announcementMVar

main :: IO ()
main = do
  serverStateMVar <- newMVar initialServerState
  let announcementStream = runWSServer serverStateMVar
      moveStream = runHTTPServer
      eventStream = moveStream `parallel` announcementStream
  runStream $ S.mapM (broadcastEvent serverStateMVar) eventStream
  _ <- getLine
  return ()
