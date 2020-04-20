{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (evaluate)
import Test.Hspec

import Data.Csv
import qualified Data.Vector as V
import qualified Data.ByteString as B
import Epidemic
import qualified Epidemic.BDSCOD as BDSCOD
import qualified Epidemic.BirthDeathSamplingCatastropheOccurrence as BDSCO
import qualified Epidemic.BirthDeathSamplingOccurrence as BDSO

p1 = Person 1
p2 = Person 2
p3 = Person 3
p4 = Person 4
p5 = Person 5
p6 = Person 6
p7 = Person 7

-- | The first set of test data does not have any catastrophe events.
demoFullEvents01 =
  [ InfectionEvent 1 p1 p2
  , InfectionEvent 2 p1 p3
  , SamplingEvent 3 p1
  , InfectionEvent 4 p2 p4
  , InfectionEvent 5 p2 p5
  , SamplingEvent 6 p4
  , InfectionEvent 7 p3 p6
  , OccurrenceEvent 8 p2
  , RemovalEvent 9 p3
  , InfectionEvent 10 p5 p7
  , OccurrenceEvent 11 p6
  , SamplingEvent 12 p5
  , RemovalEvent 13 p7
  ]

demoSampleEvents01 =
  [ InfectionEvent 1 p1 p2
  , SamplingEvent 3 p1
  , InfectionEvent 4 p2 p4
  , SamplingEvent 6 p4
  , OccurrenceEvent 8 p2
  , OccurrenceEvent 11 p6
  , SamplingEvent 12 p5
  ]

-- | The second set of test data is the same as the first but includes a
-- catastrophe event.
demoFullEvents02 =
  [ InfectionEvent 1 p1 p2
  , InfectionEvent 2 p1 p3
  , SamplingEvent 3 p1
  , InfectionEvent 4 p2 p4
  , InfectionEvent 5 p2 p5
  , SamplingEvent 6 p4
  , InfectionEvent 7 p3 p6
  , OccurrenceEvent 8 p2
  , RemovalEvent 9 p3
  , InfectionEvent 10 p5 p7
  , CatastropheEvent 11 (People $ V.singleton p5)
  , OccurrenceEvent 12 p6
  , RemovalEvent 13 p7
  ]

demoSampleEvents02 =
  [ InfectionEvent 1 p1 p2
  , SamplingEvent 3 p1
  , InfectionEvent 4 p2 p4
  , SamplingEvent 6 p4
  , OccurrenceEvent 8 p2
  , CatastropheEvent 11 (People $ V.singleton p5)
  , OccurrenceEvent 12 p6
  ]


-- | Another test set to test that catastrophes are handled correctly.
demoFullEvents03 =
  [ InfectionEvent 1 p1 p4
  , InfectionEvent 2 p1 p2
  , SamplingEvent 3 p1
  , InfectionEvent 4 p2 p3
  , InfectionEvent 5 p4 p5
  , CatastropheEvent 6 (People $ V.fromList [p2,p3,p4])
  ]

demoSampleEvents03 =
  [ InfectionEvent 1 p1 p4
  , InfectionEvent 2 p1 p2
  , SamplingEvent 3 p1
  , InfectionEvent 4 p2 p3
  , CatastropheEvent 6 (People $ V.fromList [p2,p3,p4])
  ]


-- | Another test to make sure that disasters are handled.
demoFullEvents04 =
  [ InfectionEvent 1 p1 p4
  , InfectionEvent 2 p1 p2
  , SamplingEvent 3 p1
  , InfectionEvent 4 p2 p3
  , InfectionEvent 5 p4 p5
  , CatastropheEvent 6 (People $ V.fromList [p2,p3,p4])
  , InfectionEvent 7 p5 p6
  , InfectionEvent 8 p5 p7
  , DisasterEvent 9 (People $ V.fromList [p5,p6])
  ]

demoSampleEvents04 =
  [ InfectionEvent 1 p1 p4
  , InfectionEvent 2 p1 p2
  , SamplingEvent 3 p1
  , InfectionEvent 4 p2 p3
  , CatastropheEvent 6 (People $ V.fromList [p2,p3,p4])
  , DisasterEvent 9 (People $ V.fromList [p5,p6])
  ]


main :: IO ()
main = hspec $ do
  describe "Post-simulation processing" $ do
    it "Extracting observed events" $ do
      (demoSampleEvents01 == BDSO.birthDeathSamplingOccurrenceObservedEvents demoFullEvents01) `shouldBe` True
      (demoSampleEvents02 == BDSO.birthDeathSamplingOccurrenceObservedEvents demoFullEvents02) `shouldBe` True
  describe "Catastrophe definitions" $ do
    it "Check we can find a catastrophe" $ do
      (noScheduledEvent 0 1 []) `shouldBe` True
      (noScheduledEvent 0 1 [(2,0.5)]) `shouldBe` True
      (noScheduledEvent 0 1 [(0.5,0.5)]) `shouldBe` False
      (noScheduledEvent 0 1 [(2,0.6),(0.5,0.5)]) `shouldBe` False
    it "Check we can find a particular catastrophe" $ do
      (firstScheduled 1 []) `shouldBe` Nothing
      (firstScheduled 1 [(2,0.5)]) `shouldBe` Just (2,0.5)
      (firstScheduled 1 [(0.5,0.5)]) `shouldBe` Nothing
      (firstScheduled 1 [(2,0.6),(0.5,0.5)]) `shouldBe` Just (2,0.6)
      (firstScheduled 1 [(2,0.6),(0.5,0.5),(1.5,0.4)]) `shouldBe` Just (1.5,0.4)
    it "Works on a very specific case it seems to not like" $ do
      (noScheduledEvent 2.28 (2.28+0.42) [(2.3,0.9)]) `shouldBe` False
    it "Catastrophes are handled correctly" $ do
      (demoSampleEvents03 == BDSCO.observedEvents demoFullEvents03) `shouldBe` True
    it "Catastrophes can be simulated" $ do
      demoSim <- BDSCO.simulation $ BDSCO.configuration 4 (1.3,0.1,0.1,[(3,0.5)],0.2)
      length demoSim > 1 `shouldBe` True
  describe "Disaster definitions" $ do
    it "Disasters are handled correctly" $ do
      (demoSampleEvents04 == BDSCOD.observedEvents demoFullEvents04) `shouldBe` True
    it "Disasters can be simulated" $ do
      demoSim <- BDSCOD.simulation $ BDSCOD.configuration 4 (1.3,0.1,0.1,[(3,0.5)],0.2,[(3.5,0.5)])
      length demoSim > 1 `shouldBe` True


main' :: IO ()
main' = hspec $ do
  describe "Change Event read/write" $ do
    it "check we can writte an event" $
      let demoPerson = Person 3
          demoPersonField = toField demoPerson
          demoPersonField' = "3"
          demoEvent = RemovalEvent 1.0 demoPerson
          demoRecord = toRecord demoEvent
          demoRecord' = V.fromList ["removal", "1.0", "3", "NA"] :: Record
          (Right demoEvent') = runParser (parseRecord demoRecord) :: Either String Event
          demoRecord2 = toRecord (CatastropheEvent 1.0 (People (V.fromList [p2,p3])))
          (Right demoEvent2@(CatastropheEvent _ people2)) = runParser (parseRecord demoRecord2) :: Either String Event
          demoRecord2' = toRecord demoEvent2
       in do
        (demoPersonField' == demoPersonField) `shouldBe` True
        (demoRecord' == demoRecord) `shouldBe` True
        (demoEvent' == demoEvent) `shouldBe` True
        (demoRecord2' == demoRecord2) `shouldBe` True
        (numPeople people2 == 2) `shouldBe` True



