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
    postMove :: MoveInfo -> Handler MoveInfo
    postMove moveInfo = do
      liftIO $ putMVar moveMVar moveInfo
      return moveInfo

api :: Proxy API
api = Proxy

app :: MVar MoveInfo -> Application
app moveMVar = serve api (server moveMVar)

getMoveStream :: SerialT IO MoveInfo
getMoveStream = do
  moveMVar <- liftIO newEmptyMVar
  liftIO $ forkIO $ run 8081 (app moveMVar)
  S.repeatM $ liftIO $ takeMVar moveMVar

-- WebSockets
data Client =
  Client
    { uid :: Int
    , name :: T.Text
    , wsConn :: WS.Connection
    }

data ServerState =
  ServerState
    { clients :: [Client]
    , uidCounter :: Int
    }

initialServerState :: ServerState
initialServerState = ServerState {clients = [], uidCounter = 1}

numClients :: [Client] -> Int
numClients = length

addClient :: Client -> [Client] -> [Client]
addClient client clients = client : clients

removeClient :: Client -> [Client] -> [Client]
removeClient client = filter ((/= uid client) . uid)

broadcast :: T.Text -> [Client] -> IO ()
broadcast message clients = do
  T.putStrLn message
  forM_ clients $ \client -> WS.sendTextData (wsConn client) message

application :: MVar ServerState -> MVar T.Text -> WS.PendingConnection -> IO ()
application serverStateMVar announcementMVar pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) $ do
    msg <- WS.receiveData conn
    serverState <- readMVar serverStateMVar
    let currentUid = uidCounter serverState
        newClient = Client {uid = currentUid, name = msg, wsConn = conn}
    case msg of
      _
        | not ("Player" `T.isPrefixOf` msg) ->
          WS.sendTextData conn ("Wrong announcement" :: T.Text)
        | otherwise ->
          flip finally disconnect $ do
            modifyMVar_ serverStateMVar $ \serverState -> do
              let clients' = addClient newClient $ clients serverState
              WS.sendTextData conn $ T.pack $ "Your id is: " <> (show currentUid)
              putMVar announcementMVar $ name newClient <> " joined"
              return $
                ServerState {clients = clients', uidCounter = currentUid + 1}
            keepConnAlive newClient
        where disconnect = do
                modifyMVar_ serverStateMVar $ \serverState ->
                  let clients' = removeClient newClient $ clients serverState
                      currentUid = uidCounter serverState
                   in return $
                      ServerState {clients = clients', uidCounter = currentUid}
                putMVar announcementMVar $ name newClient <> " disconnected"

keepConnAlive :: Client -> IO ()
keepConnAlive client =
   forever $ do
     swallowTextMsg
     return ()
   where swallowTextMsg :: IO T.Text
         swallowTextMsg = WS.receiveData $ wsConn client

getAnnouncementStream :: MVar T.Text -> SerialT IO T.Text
getAnnouncementStream announcementMVar = do
  S.repeatM $ liftIO $ takeMVar announcementMVar

main :: IO ()
main = do
  serverStateMVar <- newMVar initialServerState
  announcementMVar <- newEmptyMVar
  forkIO $
    WS.runServer "127.0.0.1" 8082 $ application serverStateMVar announcementMVar
  let announcementStream = getAnnouncementStream announcementMVar
      moveStream = getMoveStream
  forkIO $ runStream $ S.mapM (print) announcementStream
  runStream $ S.mapM (print) moveStream
  _ <- getLine
  return ()
