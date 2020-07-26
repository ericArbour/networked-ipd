{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Shared where

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

data PlayerMove =
  PlayerMove PlayerId Move
  deriving (Show, Generic)

instance ToJSON PlayerMove

instance FromJSON PlayerMove

type Score = Int

type Host = String

type WsPort = Int

type HttpPort = Int

data Strategy
  = AlwaysDefect
  | AlwaysCooperate
  | Random5050
  | Random8020
  | Random9010
  | TitForTat
  | TitForTwoTats
  | Vigilante
  | ForgivingTitForTat
  | Ostracize
  deriving (Eq, Read, Show, Enum, Bounded)

data PublicEvent
  = PlayerJoin PlayerId
  | NewGame PlayerId PlayerId
  | GameResult PlayerId Move PlayerId Move
  deriving (Show, Generic)

instance ToJSON PublicEvent

instance FromJSON PublicEvent

newtype IdAssignment =
  IdAssignment PlayerId
  deriving (Show, Generic)

instance ToJSON IdAssignment

instance FromJSON IdAssignment

type API = "move" :> ReqBody '[ JSON] PlayerMove :> Post '[ JSON] NoContent
