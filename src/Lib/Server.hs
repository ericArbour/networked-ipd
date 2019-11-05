{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Lib.Server where

import Prelude ()
import Prelude.Compat
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
type Client = (T.Text, WS.Connection)

type ServerState = [Client]

newServerState :: [Client]
newServerState = []

numClients :: ServerState -> Int
numClients = length

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst)

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

broadcast :: T.Text -> ServerState -> IO ()
broadcast message clients = do
  T.putStrLn message
  forM_ clients $ \(_, conn) -> WS.sendTextData conn message

application state pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) $ do
    msg <- WS.receiveData conn
    clients <- readMVar state
    case msg of
      _
        | not (prefix `T.isPrefixOf` msg) ->
          WS.sendTextData conn ("Wrong announcement" :: T.Text)
        | any ($ fst client) [T.null, T.any isPunctuation, T.any isSpace] ->
          WS.sendTextData
            conn
            ("Name cannot " <> "contain punctuation or whitespace, and " <>
             "cannot be empty" :: T.Text)
        | clientExists client clients ->
          WS.sendTextData conn ("User already exists" :: T.Text)
        | otherwise ->
          flip finally disconnect $ do
            modifyMVar_ state $ \s -> do
              let s' = addClient client s
              WS.sendTextData conn $
                "Welcome! Users: " <> T.intercalate ", " (map fst s)
              broadcast (fst client <> " joined") s'
              return s'
            talk client state
        where prefix = "Hi! I am "
              client = (T.drop (T.length prefix) msg, conn)
              disconnect = do
                s <-
                  modifyMVar state $ \s ->
                    let s' = removeClient client s
                     in return (s', s')
                broadcast (fst client <> " disconnected") s

talk :: Client -> MVar ServerState -> IO ()
talk (user, conn) state =
  forever $ do
    msg <- WS.receiveData conn
    readMVar state >>= broadcast (user `mappend` ": " `mappend` msg)

main :: IO ()
main = do
  state <- newMVar newServerState
  forkIO $ WS.runServer "127.0.0.1" 8082 $ application state
  runStream $ S.mapM (print) $ getMoveStream
  _ <- getLine
  return ()
