{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Lib.Shared where

import Data.Aeson.Types (FromJSON, ToJSON)
import GHC.Generics (Generic)
import qualified Network.WebSockets as WS
import Servant

data Move
  = Cooperate
  | Defect
  deriving (Eq, Show, Generic)

instance ToJSON Move

instance FromJSON Move

type PlayerId = Int

data MovePost =
  MovePost PlayerId Move
  deriving (Show, Generic)

instance ToJSON MovePost

instance FromJSON MovePost

data Strategy =
  Default
  deriving (Eq, Show, Generic)

instance ToJSON Strategy

instance FromJSON Strategy

type Score = Int

data PublicEvent
  = PlayerJoin PlayerId Strategy
  | PlayerQuit PlayerId
  | PlayerMove PlayerId Move
  | NewGame PlayerId PlayerId
  | GameResult [(PlayerId, Score)]
  deriving (Show, Generic)

instance ToJSON PublicEvent

instance FromJSON PublicEvent

newtype IdAssignment =
  IdAssignment PlayerId
  deriving (Show, Generic)

instance ToJSON IdAssignment

instance FromJSON IdAssignment

type API = "move" :> ReqBody '[ JSON] MovePost :> Post '[ JSON] NoContent
