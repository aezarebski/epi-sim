import Test.Hspec
import Control.Exception (evaluate)

import qualified Data.Vector as V
import Epidemic
import Epidemic.BirthDeathSamplingOccurrence

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

main :: IO ()
main = hspec $ do
  describe "Post-simulation processing" $ do
    it "Extracting observed events" $ do
      (demoSampleEvents01 == birthDeathSamplingOccurrenceObservedEvents demoFullEvents01) `shouldBe` True
      (demoSampleEvents02 == birthDeathSamplingOccurrenceObservedEvents demoFullEvents02) `shouldBe` True
  describe "Catastrophe definitions" $ do
    it "Check we can find a catastrophe" $ do
      (noCatastrophe 0 1 []) `shouldBe` True
      (noCatastrophe 0 1 [(2,0.5)]) `shouldBe` True
      (noCatastrophe 0 1 [(0.5,0.5)]) `shouldBe` False
      (noCatastrophe 0 1 [(2,0.6),(0.5,0.5)]) `shouldBe` False
    it "Check we can find a particular catastrophe" $ do
      (firstCatastrophe 1 []) `shouldBe` Nothing
      (firstCatastrophe 1 [(2,0.5)]) `shouldBe` Just (2,0.5)
      (firstCatastrophe 1 [(0.5,0.5)]) `shouldBe` Nothing
      (firstCatastrophe 1 [(2,0.6),(0.5,0.5)]) `shouldBe` Just (2,0.6)
      (firstCatastrophe 1 [(2,0.6),(0.5,0.5),(1.5,0.4)]) `shouldBe` Just (1.5,0.4)
    it "Works on a very specific case it seems to not like" $ do
      (noCatastrophe 2.28 (2.28+0.42) [(2.3,0.9)]) `shouldBe` False
