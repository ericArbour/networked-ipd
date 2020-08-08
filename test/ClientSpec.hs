module ClientSpec where

import Data.Map.Strict (fromList)
import Test.Hspec

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M

import Client
  ( MoveAgainst(..)
  , eightyTwentyOutcomes
  , fiftyFiftyOutcomes
  , ninetyTenOutcomes
  , ostracize
  , reaction
  , titForTat
  , titForTwoTats
  , vigilante
  )
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

opOnlyCooperates =
  [ MoveAgainst myId Cooperate
  , MoveAgainst opId Cooperate
  , MoveAgainst myId Cooperate
  ]

opFirstGameMap = M.fromList []

opHasHistoryMap = M.fromList [(2, [MoveAgainst myId Defect])]

getElemPercentage :: Eq a => a -> NE.NonEmpty a -> Float
getElemPercentage x xs =
  fromIntegral (length (NE.filter (== x) xs)) / fromIntegral (NE.length xs)

spec = do
  describe "titForTat strategy" $ do
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
  describe "titForTwoTat strategy" $ do
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
  describe "vigilante strategy" $ do
    it "cooperates on opponent's first game" $
      vigilante opFirstGame `shouldBe` Cooperate
    it "cooperates when opponent last cooperated with acting player" $
      vigilante opCooperateMeLast `shouldBe` Cooperate
    it "defects when opponent last defected against acting player" $
      vigilante opDefectMeLast `shouldBe` Defect
    it "cooperates when opponent last cooperated with another player" $
      vigilante opDefectMeNotOther `shouldBe` Cooperate
    it "defects when opponent last defected against another player" $
      vigilante opCooperateMeNotOther `shouldBe` Defect
  describe "ostracize strategy" $ do
    it "cooperates on opponent's first game" $
      ostracize opFirstGame `shouldBe` Cooperate
    it "cooperates if opponent has only cooperated" $
      ostracize opOnlyCooperates `shouldBe` Cooperate
    it "defects if opponent has defected against acting player" $
      ostracize opDefectMeNotOther `shouldBe` Defect
    it "defects if opponent has defected against another player" $
      ostracize opCooperateMeNotOther `shouldBe` Defect
  describe "reaction utility" $ do
    it "cooperates on opponent's first game" $
      reaction opFirstGameMap opId (const Defect) `shouldBe` Cooperate
    it "runs strategy function if opponent has a move history" $
      reaction opHasHistoryMap opId (const Defect) `shouldBe` Defect
  describe "5050 strategy" $ do
    it "has a 50% chance of cooperating" $
      getElemPercentage Cooperate fiftyFiftyOutcomes `shouldBe` 0.5
    it "has a 50% chance of defecting" $
      getElemPercentage Defect fiftyFiftyOutcomes `shouldBe` 0.5
  describe "8020 strategy" $ do
    it "has an 80% chance of cooperating" $
      getElemPercentage Cooperate eightyTwentyOutcomes `shouldBe` 0.8
    it "has a 20% chance of defecting" $
      getElemPercentage Defect eightyTwentyOutcomes `shouldBe` 0.2
  describe "9010 strategy" $ do
    it "has a 90% chance of cooperating" $
      getElemPercentage Cooperate ninetyTenOutcomes `shouldBe` 0.9
    it "has a 10% chance of defecting" $
      getElemPercentage Defect ninetyTenOutcomes `shouldBe` 0.1
