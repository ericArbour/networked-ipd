{-# LANGUAGE OverloadedStrings #-}
module Lib.Client where


--------------------------------------------------------------------------------
import           Control.Concurrent  (forkIO, threadDelay)
import           Control.Exception   (finally)
import           Control.Monad       (forever, unless)
import           Control.Monad.Trans (liftIO)
import           Control.Monad.State
import           Network.Socket      (withSocketsDo)
import           Data.Maybe
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Network.WebSockets  as WS
import Data.Proxy
import Servant.Client
import Network.HTTP.Client (newManager, defaultManagerSettings, Manager)

import Lib.Shared

-- HTTP
-- -----------------------------------------------------------------------------
api :: Proxy API
api = Proxy

hoistHTTPClient :: Manager -> Client IO API
hoistHTTPClient manager' = hoistClient api (handleError . getIOClient) (client api)
  where baseurl = BaseUrl Http "localhost" 8081 ""
        clientEnv :: ClientEnv
        clientEnv = mkClientEnv manager' baseurl
        getIOClient :: ClientM a -> IO (Either ClientError a)
        getIOClient = flip runClientM clientEnv
        handleError :: IO (Either ClientError a) -> IO a
        handleError = fmap (either (error . show) id)

-- Websockets
--------------------------------------------------------------------------------
delay :: Int
delay = 1000000

idPrefix :: T.Text
idPrefix = "Your id is: "

parseId :: T.Text -> Maybe Int
parseId = Just . read . T.unpack . T.replace idPrefix ""

wsHandler :: Client IO API -> WS.Connection -> StateT (Maybe Int) IO ()
wsHandler postMove conn = forever $ do
  msg <- liftIO $ WS.receiveData conn
  maybeId <- get
  liftIO $ print maybeId
  when (idPrefix `T.isPrefixOf` msg && isNothing maybeId) (put $ parseId msg)
  liftIO $ T.putStrLn msg
  liftIO $ threadDelay delay
  maybeId <- get
  liftIO $ postMove $ MoveInfo { userId = fromJust maybeId, move = Defect }
  return () 

app :: Client IO API -> WS.ClientApp ()
app postMove conn = do
    putStrLn "Connected!"
    WS.sendTextData conn ("Player1" :: T.Text)

    flip finally disconnect $ (evalStateT (wsHandler postMove conn) Nothing)
    where disconnect = WS.sendClose conn ("Bye!" :: Text)


--------------------------------------------------------------------------------
main2 :: IO ()
main2 = do
  manager' <- newManager defaultManagerSettings
  let postMove = hoistHTTPClient manager'
  withSocketsDo $ WS.runClient "127.0.0.1" 8082 "/" (app postMove) 
