import Test.Hspec
import Control.Exception (evaluate)

import Control.Monad
import Data.Maybe (isJust, fromJust)
import qualified Data.Vector as V
import Epidemic
import Epidemic.Utility
import qualified Epidemic.BirthDeath as BD
import qualified Epidemic.BirthDeathSamplingOccurrence as BDSO
import qualified Epidemic.BirthDeathSamplingCatastropheOccurrence as BDSCO
import qualified Epidemic.BDSCOD as BDSCOD


withinNPercent n x y = x - d < y && y < x + d where d = n * x / 100


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



eventHandlingTests = do
  describe "Post-simulation processing" $ do
    it "Extracting observed events" $ do
      (demoSampleEvents01 == BDSO.observedEvents demoFullEvents01) `shouldBe` True
      (demoSampleEvents02 == BDSO.observedEvents demoFullEvents02) `shouldBe` True
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
      demoSim <- simulation (BDSCO.configuration 4 (1.3,0.1,0.1,[(3,0.5)],0.2)) BDSCO.allEvents
      length demoSim > 1 `shouldBe` True
  describe "Disaster definitions" $ do
    it "Disasters are handled correctly" $ do
      (demoSampleEvents04 == BDSCOD.observedEvents demoFullEvents04) `shouldBe` True
    it "Disasters can be simulated" $ do
      demoSim <- simulation (BDSCOD.configuration 4 (1.3,0.1,0.1,[(3,0.5)],0.2,[(3.5,0.5)])) BDSCOD.allEvents
      length demoSim > 1 `shouldBe` True



birthDeathTests = do
  describe "BirthDeath module tests" $ do
    it "Construct a simulation configuration" $ do
      (isJust (BD.configuration 1 (1,1))) `shouldBe` True
      (isJust (BD.configuration (-1) (1,1))) `shouldBe` False
      (isJust (BD.configuration 1 ((-1),1))) `shouldBe` False
      (isJust (BD.configuration 1 (1,(-1)))) `shouldBe` False
      (isJust (BD.configuration 1 ((-1),(-1)))) `shouldBe` False
    it "Mean behaviour is approximately correct" $
      let mean xs = fromIntegral (sum xs) / (fromIntegral $ length xs)
          meanFinalSize = exp ((2.1 - 0.2) * 1.5)
          randomBDEvents = simulationWithSystemRandom (fromJust $ BD.configuration 1.5 (2.1, 0.2)) BD.allEvents
          numRepeats = 1000
       in do
        finalSizes <- replicateM numRepeats (finalSize <$> randomBDEvents)
        (withinNPercent 5 (mean finalSizes) meanFinalSize) `shouldBe` True

main :: IO ()
main = hspec $ do
  eventHandlingTests
  birthDeathTests
