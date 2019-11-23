{-# LANGUAGE OverloadedStrings #-}
module Lib.Client where


--------------------------------------------------------------------------------
import           Control.Concurrent  (forkIO, threadDelay)
import           Control.Concurrent.MVar
import           Control.Exception   (finally, Exception, throw)
import           Control.Monad       (forever, unless)
import           Control.Monad.Trans (liftIO)
import           Control.Monad.State
import           Network.Socket      (withSocketsDo)
import           Data.Maybe
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import Data.Typeable                 (Typeable)
import qualified Network.WebSockets  as WS
import Data.Proxy
import Servant.Client
import Network.HTTP.Client (newManager, defaultManagerSettings, Manager)
import Streamly
import qualified Streamly.Prelude as S

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

data GameException = InvalidStrategy | CannotParseId deriving (Show, Typeable)

instance Exception GameException

idPrefix :: T.Text
idPrefix = "Your id is: "

parseId :: T.Text -> Maybe Int
parseId = Just . read . T.unpack . T.replace idPrefix ""

fakeGameLogic :: Client IO API -> T.Text -> StateT (Maybe Int) IO ()
fakeGameLogic postMove event = do
  liftIO $ T.putStrLn event
  liftIO $ threadDelay delay
  maybeId <- get
  let myId = fromJust maybeId
  liftIO $ postMove $ MoveInfo { userId = myId, move = Defect }
  return ()

eventHandler :: Client IO API -> T.Text -> StateT (Maybe Int) IO ()
eventHandler postMove event = do
  maybeId <- get
  case event of
    _ | isNothing maybeId && not (idPrefix `T.isPrefixOf` event) -> throw InvalidStrategy
      | isNothing maybeId && idPrefix `T.isPrefixOf` event -> do 
          put $ parseId event
          fakeGameLogic postMove event
      | otherwise -> fakeGameLogic postMove event

wsClient :: MVar T.Text -> WS.ClientApp ()
wsClient eventMVar conn = do
    putStrLn "Connected!"
    WS.sendTextData conn ("Default Strategy" :: T.Text)
    flip finally disconnect $ forever $ do
      msg <- WS.receiveData conn
      putMVar eventMVar msg
    where disconnect = WS.sendClose conn ("Bye!" :: T.Text)

getEventStream :: SerialT (StateT (Maybe Int) IO) T.Text
getEventStream = do
  eventMVar <- liftIO newEmptyMVar
  liftIO $ forkIO $ withSocketsDo $ WS.runClient "127.0.0.1" 8082 "/" (wsClient eventMVar)
  S.repeatM $ liftIO $ takeMVar eventMVar
--------------------------------------------------------------------------------
main2 :: IO ()
main2 = do
  manager' <- newManager defaultManagerSettings
  let postMove = hoistHTTPClient manager'
  evalStateT (runStream $ S.mapM (eventHandler postMove) $ getEventStream) Nothing
