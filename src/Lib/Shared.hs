{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Lib.Shared where

import Data.Aeson.Types (FromJSON, ToJSON)
import GHC.Generics (Generic)
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

data Strategy =
  Default
  deriving (Eq, Show, Generic)

instance ToJSON Strategy

instance FromJSON Strategy

data Event
  = Move PlayerId Move
  | Join PlayerId Strategy
  | Leave PlayerId
  | GameStart PlayerId PlayerId
  deriving (Show, Generic)

instance ToJSON Event

instance FromJSON Event

newtype IdAssignment =
  IdAssignment PlayerId
  deriving (Show, Generic)

instance ToJSON IdAssignment

instance FromJSON IdAssignment

type API = "move" :> ReqBody '[ JSON] PlayerMove :> Post '[ JSON] NoContent
