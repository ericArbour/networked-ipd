module ClientSpec where

import Data.Map.Strict (fromList)
import Test.Hspec

import Client (MoveAgainst(..), titForTat, titForTwoTats)
import Shared (Move(..))

myId = 1

opId = 2

otherId = 3

opFirstGame = []

opCooperateMeOnly = [MoveAgainst myId Cooperate]

opDefectMeOnly = [MoveAgainst myId Defect]

opCooperateMeLast = [MoveAgainst myId Cooperate, MoveAgainst myId Defect]

opDefectMeLast = [MoveAgainst myId Defect, MoveAgainst myId Cooperate]

opCooperateMeNotOther = [MoveAgainst otherId Defect, MoveAgainst myId Cooperate]

opDefectMeNotOther = [MoveAgainst otherId Cooperate, MoveAgainst myId Defect]

opDefectMeTwiceOnly = [MoveAgainst myId Defect, MoveAgainst myId Defect]

opDefectMeTwiceNotOther =
  [ MoveAgainst otherId Cooperate
  , MoveAgainst myId Defect
  , MoveAgainst myId Defect
  ]

opNeverRepeatsDefect =
  [MoveAgainst myId Defect, MoveAgainst myId Cooperate, MoveAgainst myId Defect]

spec = do
  describe "titForTat" $ do
    it "cooperates on opponent's first game" $
      titForTat myId opFirstGame `shouldBe` Cooperate
    it "cooperates if opponent has only cooperated with acting player" $
      titForTat myId opCooperateMeOnly `shouldBe` Cooperate
    it "defects if opponent has only defected against acting player" $
      titForTat myId opDefectMeOnly `shouldBe` Defect
    it "cooperates if opponent cooperated with acting player last" $
      titForTat myId opCooperateMeLast `shouldBe` Cooperate
    it "defects if opponent defected against acting player last" $
      titForTat myId opDefectMeLast `shouldBe` Defect
    it
      ("cooperates if opponent cooperated with acting player last, " <>
       "ignoring more recent moves against other players") $
      titForTat myId opCooperateMeNotOther `shouldBe` Cooperate
    it
      ("defects if opponent cooperated with acting player last, " <>
       "ignoring more recent moves against other players") $
      titForTat myId opDefectMeNotOther `shouldBe` Defect
  describe "titForTwoTat" $ do
    it "cooperates on opponent's first game" $
      titForTwoTats myId opFirstGame `shouldBe` Cooperate
    it "cooperates if opponent has only cooperated with acting player" $
      titForTwoTats myId opCooperateMeOnly `shouldBe` Cooperate
    it "cooperates if opponent has only defected against acting player" $
      titForTwoTats myId opDefectMeOnly `shouldBe` Cooperate
    it "cooperates if opponent cooperated with acting player last" $
      titForTwoTats myId opCooperateMeLast `shouldBe` Cooperate
    it
      "cooperates if opponent defected against acting player last and only once" $
      titForTwoTats myId opDefectMeLast `shouldBe` Cooperate
    it
      ("cooperates if opponent cooperated with acting player last, " <>
       "ignoring more recent moves against other plalyers") $
      titForTwoTats myId opCooperateMeNotOther `shouldBe` Cooperate
    it
      ("cooperates if opponent cooperated with acting player last and only once, " <>
       "ignoring more recent moves against other players") $
      titForTwoTats myId opDefectMeNotOther `shouldBe` Cooperate
    it "defects if opponent has only defected against acting player twice" $
      titForTwoTats myId opDefectMeTwiceOnly `shouldBe` Defect
    it
      ("defects if opponent has defected against acting player twice, " <>
       "ignoring more recent moves against other players") $
      titForTwoTats myId opDefectMeTwiceOnly `shouldBe` Defect
    it "defects only if opponent defects twice in a row" $
      titForTwoTats myId opNeverRepeatsDefect `shouldBe` Cooperate
