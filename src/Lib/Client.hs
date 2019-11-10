{-# LANGUAGE OverloadedStrings #-}
module Lib.Client where


--------------------------------------------------------------------------------
import           Control.Concurrent  (forkIO, threadDelay)
import           Control.Monad       (forever, unless)
import           Control.Monad.Trans (liftIO)
import           Network.Socket      (withSocketsDo)
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

getPostMove :: Manager -> Client IO API
getPostMove manager' = hoistClient api (handleError . getIOClient) (client api)
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

app :: WS.ClientApp ()
app conn = do
    putStrLn "Connected!"

    -- Fork a thread that writes WS data to stdout
    _ <- forkIO $ forever $ do
        msg <- WS.receiveData conn
        liftIO $ T.putStrLn msg

    WS.sendTextData conn ("Player1" :: T.Text)
    -- Read from stdin and write to WS
    let loop = do
            liftIO $ threadDelay delay
            WS.sendTextData conn ("Test 1 2 3" :: T.Text) >> loop

    loop
    WS.sendClose conn ("Bye!" :: Text)


--------------------------------------------------------------------------------
main2 :: IO ()
main2 = do
  manager' <- newManager defaultManagerSettings
  let postMove = getPostMove manager'
  res <- postMove $ MoveInfo { userId = 1, move = Defect }
  print res
  withSocketsDo $ WS.runClient "127.0.0.1" 8082 "/" app
