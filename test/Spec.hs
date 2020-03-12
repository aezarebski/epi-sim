import Test.Hspec
import Control.Exception (evaluate)

import Epidemic
import Epidemic.BirthDeathSamplingOccurrence

p1 = Person 1
p2 = Person 2
p3 = Person 3
p4 = Person 4
p5 = Person 5
p6 = Person 6
p7 = Person 7

demoFullEvents = [ InfectionEvent 1 p1 p2
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
                 , RemovalEvent 13 p7]

demoSampleEvents = [ InfectionEvent 1 p1 p2
                   , SamplingEvent 3 p1
                   , InfectionEvent 4 p2 p4
                   , SamplingEvent 6 p4
                   , OccurrenceEvent 8 p2
                   , OccurrenceEvent 11 p6
                   , SamplingEvent 12 p5]



main :: IO ()
main = hspec $ do
  describe "Post-simulation processing" $ do
    it "Extracting observed events" $ do
      (demoSampleEvents == birthDeathSamplingOccurrenceObservedEvents demoFullEvents) `shouldBe` True
