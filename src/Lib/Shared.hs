{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Lib.Shared
  ( MoveInfo(..)
  , Move(..)
  , API
  ) where

import Data.Aeson.Types (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Servant

data Move
  = Cooperate
  | Defect
  deriving (Eq, Show, Generic)

instance ToJSON Move

instance FromJSON Move

data MoveInfo =
  MoveInfo
    { userId :: Int
    , move :: Move
    }
  deriving (Eq, Show, Generic)

instance ToJSON MoveInfo

instance FromJSON MoveInfo

type API = "move" :> ReqBody '[ JSON] MoveInfo :> Post '[ JSON] NoContent
