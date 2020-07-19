module ClientSpec where

import Data.Map.Strict (fromList)
import Test.Hspec

import Client
import Shared (Move(..), PlayerId, Strategy(..))

myId :: PlayerId
myId = 1

opId :: PlayerId
opId = 2

otherId :: PlayerId
otherId = 3

opFirstGame :: MoveMap
opFirstGame = fromList []

opDefectMeLast :: MoveMap
opDefectMeLast = fromList [(opId, [MoveAgainst myId Defect])]

opCooperateMeLast :: MoveMap
opCooperateMeLast = fromList [(opId, [MoveAgainst myId Cooperate])]

opDefectMeLast2Times :: MoveMap
opDefectMeLast2Times =
  fromList [(opId, [MoveAgainst myId Defect, MoveAgainst myId Defect])]

spec = do
  describe "getMove with Strategy Defect" $ do
    let getDefect = getMove AlwaysDefect myId opId
    move <- runIO $ getDefect opFirstGame
    it "defects on opponent's first game" $ move `shouldBe` Defect
    move <- runIO $ getDefect opDefectMeLast
    it "defects when opponent defected against me last" $ move `shouldBe` Defect
    move <- runIO $ getDefect opCooperateMeLast
    it "defects when opponent cooperated with me last" $ move `shouldBe` Defect
    move <- runIO $ getDefect opDefectMeLast2Times
    it "defects when opponent defected against me the last two times" $
      move `shouldBe` Defect
