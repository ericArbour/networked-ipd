{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Lib.Shared where

import Servant
import Data.Aeson
import qualified Data.Aeson.Parser
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import GHC.Generics

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

