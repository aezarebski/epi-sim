{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (evaluate)
import Control.Monad
import qualified Data.ByteString as B
import Data.Csv
import Data.Maybe (fromJust, isJust, isNothing)
import qualified Data.Vector as V
import Epidemic
import Epidemic.Types.Population
import Epidemic.Types.Events
import Epidemic.Types.Parameter
import qualified Epidemic.BDSCOD as BDSCOD
import qualified Epidemic.BirthDeath as BD
import qualified Epidemic.BirthDeathSamplingCatastropheOccurrence as BDSCO
import qualified Epidemic.BirthDeathSamplingOccurrence as BDSO
import qualified Epidemic.InhomogeneousBDS as InhomBDS
import Epidemic.Utility
import Statistics.Sample
import qualified System.Random.MWC as MWC
import Test.Hspec

-- | y is within n% of x from x.
withinNPercent n x y = x - d < y && y < x + d
  where
    d = n * x / 100

p1 = Person 1

p2 = Person 2

p3 = Person 3

p4 = Person 4

p5 = Person 5

p6 = Person 6

p7 = Person 7

-- | The first set of test data does not have any catastrophe events.
demoFullEvents01 =
  [ Infection 1 p1 p2
  , Infection 2 p1 p3
  , Sampling 3 p1
  , Infection 4 p2 p4
  , Infection 5 p2 p5
  , Sampling 6 p4
  , Infection 7 p3 p6
  , Occurrence 8 p2
  , Removal 9 p3
  , Infection 10 p5 p7
  , Occurrence 11 p6
  , Sampling 12 p5
  , Removal 13 p7
  ]

demoSampleEvents01 =
  [ Infection 1 p1 p2
  , Sampling 3 p1
  , Infection 4 p2 p4
  , Sampling 6 p4
  , Occurrence 8 p2
  , Occurrence 11 p6
  , Sampling 12 p5
  ]

-- | The second set of test data is the same as the first but includes a
-- catastrophe event.
demoFullEvents02 =
  [ Infection 1 p1 p2
  , Infection 2 p1 p3
  , Sampling 3 p1
  , Infection 4 p2 p4
  , Infection 5 p2 p5
  , Sampling 6 p4
  , Infection 7 p3 p6
  , Occurrence 8 p2
  , Removal 9 p3
  , Infection 10 p5 p7
  , Catastrophe 11 (asPeople [p5])
  , Occurrence 12 p6
  , Removal 13 p7
  ]

demoSampleEvents02 =
  [ Infection 1 p1 p2
  , Sampling 3 p1
  , Infection 4 p2 p4
  , Sampling 6 p4
  , Occurrence 8 p2
  , Catastrophe 11 (asPeople [p5])
  , Occurrence 12 p6
  ]

-- | Another test set to test that catastrophes are handled correctly.
demoFullEvents03 =
  [ Infection 1 p1 p4
  , Infection 2 p1 p2
  , Sampling 3 p1
  , Infection 4 p2 p3
  , Infection 5 p4 p5
  , Catastrophe 6 (asPeople [p2, p3, p4])
  ]

demoSampleEvents03 =
  [ Infection 1 p1 p4
  , Infection 2 p1 p2
  , Sampling 3 p1
  , Infection 4 p2 p3
  , Catastrophe 6 (asPeople [p2, p3, p4])
  ]

-- | Another test to make sure that disasters are handled.
demoFullEvents04 =
  [ Infection 1 p1 p4
  , Infection 2 p1 p2
  , Sampling 3 p1
  , Infection 4 p2 p3
  , Infection 5 p4 p5
  , Catastrophe 6 (asPeople [p2, p3, p4])
  , Infection 7 p5 p6
  , Infection 8 p5 p7
  , Disaster 9 (asPeople [p5, p6])
  ]

demoSampleEvents04 =
  [ Infection 1 p1 p4
  , Infection 2 p1 p2
  , Sampling 3 p1
  , Infection 4 p2 p3
  , Catastrophe 6 (asPeople [p2, p3, p4])
  , Disaster 9 (asPeople [p5, p6])
  ]

eventHandlingTests = do
  describe "Post-simulation processing" $ do
    it "Extracting observed events" $ do
      (demoSampleEvents01 == BDSO.observedEvents demoFullEvents01) `shouldBe`
        True
      (demoSampleEvents02 == BDSO.observedEvents demoFullEvents02) `shouldBe`
        True
      let demoEvents = [Catastrophe 0.5  (asPeople []) -- Because the first event is a null event it can be ignored!
                       ,Infection 1.0 p1 p2
                       ,Catastrophe 1.5 (asPeople [])
                       ,Catastrophe 2.0 (asPeople [p1,p2])]
      (length demoEvents == 4) `shouldBe` True
      ((length <$> BDSCOD.observedEvents (tail demoEvents)) == (Just 2)) `shouldBe` True
      ((length <$> BDSCOD.observedEvents (demoEvents)) == (Just 2)) `shouldBe` True
      (BDSCOD.observedEvents (demoEvents) == BDSCOD.observedEvents (tail demoEvents)) `shouldBe` True
      (maybeEpidemicTree (demoEvents) == maybeEpidemicTree (tail demoEvents)) `shouldBe` True
  describe "Catastrophe definitions" $ do
    it "Check we can find a catastrophe" $ do
      (noScheduledEvent 0 1 (Timed [])) `shouldBe` True
      (noScheduledEvent 0 1 (Timed [(2, 0.5)])) `shouldBe` True
      (noScheduledEvent 0 1 (Timed [(0.5, 0.5)])) `shouldBe` False
      (noScheduledEvent 0 1 (Timed [(2, 0.6), (0.5, 0.5)])) `shouldBe` False
    it "Check we can find a particular catastrophe" $ do
      (firstScheduled 1 (Timed [])) `shouldBe` Nothing
      (firstScheduled 1 (Timed [(2, 0.5)])) `shouldBe` Just (2, 0.5)
      (firstScheduled 1 (Timed [(0.5, 0.5)])) `shouldBe` Nothing
      (firstScheduled 1 (Timed [(2, 0.6), (0.5, 0.5)])) `shouldBe` Just (2, 0.6)
      isNothing (asTimed [(2, 0.6 :: Rate), (0.5, 0.5), (1.5, 0.4)]) `shouldBe` True
      (firstScheduled 1 (Timed [(2, 0.6), (0.5, 0.5), (1.5, 0.4)])) `shouldBe`
        Just (2, 0.6)
    it "Works on a very specific case it seems to not like" $ do
      (noScheduledEvent 2.28 (2.28 + 0.42) (Timed [(2.3, 0.9)])) `shouldBe` False
    it "Catastrophes are handled correctly" $ do
      (demoSampleEvents03 == BDSCO.observedEvents demoFullEvents03) `shouldBe`
        True
    it "Catastrophes can be simulated" $ do
      demoSim <-
        simulation False
          (fromJust (BDSCO.configuration 4 (1.3, 0.1, 0.1, ([(3, 0.5)]), 0.2)))
          BDSCO.allEvents
      length demoSim > 1 `shouldBe` True
  describe "Disaster definitions" $ do
    it "Disasters are handled correctly" $ do
      (demoSampleEvents04 == fromJust (BDSCOD.observedEvents demoFullEvents04)) `shouldBe`
        True
    it "Disasters can be simulated" $ do
      demoSim <-
        simulation False
          (fromJust (BDSCOD.configuration 4 (1.3, 0.1, 0.1, [(3, 0.5)], 0.2, [(3.5, 0.5)])))
          BDSCOD.allEvents
      length demoSim > 1 `shouldBe` True

birthDeathTests = do
  describe "BirthDeath module tests" $ do
    it "Construct a simulation configuration" $ do
      (isJust (BD.configuration 1 (1, 1))) `shouldBe` True
      (isJust (BD.configuration (-1) (1, 1))) `shouldBe` False
      (isJust (BD.configuration 1 ((-1), 1))) `shouldBe` False
      (isJust (BD.configuration 1 (1, (-1)))) `shouldBe` False
      (isJust (BD.configuration 1 ((-1), (-1)))) `shouldBe` False
    it "Mean behaviour is approximately correct" $
      let mean xs = fromIntegral (sum xs) / (fromIntegral $ length xs)
          meanFinalSize = exp ((2.1 - 0.2) * 1.5)
          randomBDEvents =
            simulationWithSystemRandom False
              (fromJust $ BD.configuration 1.5 (2.1, 0.2))
              BD.allEvents
          numRepeats = 3000
       in do finalSizes <- replicateM numRepeats (finalSize <$> randomBDEvents)
             (withinNPercent 5 (mean finalSizes) meanFinalSize) `shouldBe` True

helperFuncTests = do
  describe "Helpers in Utility" $ do
    it "the isAscending function works" $ do
      (isAscending ([] :: [Time])) `shouldBe` True
      (isAscending [-1.0]) `shouldBe` True
      (isAscending [1.0]) `shouldBe` True
      (isAscending [1.0, 2.0]) `shouldBe` True
      (isAscending [1.0, 2.0, 3.0]) `shouldBe` True
      (isAscending [1.0, -2.0]) `shouldBe` False
      (isAscending [1.0, -2.0, 3.0]) `shouldBe` False
      (isAscending [1.0, 2.0, -3.0]) `shouldBe` False
    it "the asTimed function works" $ do
      (isJust $ asTimed []) `shouldBe` True
      (isJust $ asTimed [(0, 1)]) `shouldBe` True
      (isJust $ asTimed [(0, 1), (1, 3)]) `shouldBe` True
      (isJust $ asTimed [(0, 3), (1, 1)]) `shouldBe` True
      (isJust $ asTimed [(1, 3), (0, 1)]) `shouldBe` False
    let demoTimed = fromJust $ asTimed [(0, 1.2), (1, 3.1), (2, 2.7)]
     in do it "the cadlagValue function works" $ do
             (isJust $ cadlagValue demoTimed (-1.0)) `shouldBe` False
             ((== 1.2) . fromJust $ cadlagValue demoTimed 0.0) `shouldBe` True
             ((== 1.2) . fromJust $ cadlagValue demoTimed 0.5) `shouldBe` True
             ((== 3.1) . fromJust $ cadlagValue demoTimed 1.5) `shouldBe` True
           it "the diracDeltaValue function works" $ do
             ((== 1.2) . fromJust $ diracDeltaValue demoTimed 0) `shouldBe` True
             (isJust $ diracDeltaValue demoTimed 1) `shouldBe` True
             (isJust $ diracDeltaValue demoTimed 0.9) `shouldBe` False
             (isJust $ diracDeltaValue demoTimed 1.1) `shouldBe` False
           it "the hasTime function works" $ do
             (hasTime demoTimed 0) `shouldBe` True
             (hasTime demoTimed 0.5) `shouldBe` False
             (hasTime demoTimed 1) `shouldBe` True
             (hasTime demoTimed 1.5) `shouldBe` False
           it "the nextTime function works" $ do
             (0 == (fromJust $ nextTime demoTimed (-1))) `shouldBe` True
             (1 == (fromJust $ nextTime demoTimed (0))) `shouldBe` True
             (1 == (fromJust $ nextTime demoTimed (0.5))) `shouldBe` True
           it "the nextTime function handles the last time correctly" $ do
             isJust (nextTime demoTimed 1.9) `shouldBe` True
             isJust (nextTime demoTimed 2.0) `shouldBe` True
             isJust (nextTime demoTimed 2.1) `shouldBe` True
             isJust (nextTime demoTimed 10.0) `shouldBe` True
    it "shifted times work" $
      let sf = fromJust $ asTimed [(-1.0,2.0),(1,3.0)]
          val1 = cadlagValue sf 0
          val2 = cadlagValue sf (-2.0)
          val3 = cadlagValue sf 1.5
       in do
        isJust val1 `shouldBe` True
        val1 == Just 2.0 `shouldBe` True
        (not $ isJust val2) `shouldBe` True
        isJust val3 `shouldBe` True
        val3 == Just 3.0 `shouldBe` True
    it "the asTimed function returns nothing as expected" $ do
      (isJust $ asTimed [(0.0,-1)]) `shouldBe` True
      (isJust $ asTimed [(0.0,1),(1.0,-1)]) `shouldBe` True
      (isJust $ InhomBDS.inhomBDSRates [(0.0,1),(1.0,-1)] 0.5 0.5) `shouldBe` False



readwriteTests =
  do
    describe "Change Event read/write" $ do
      it "check we can writte an event" $
        let demoPerson = Person 3
            demoPersonField = toField demoPerson
            demoPersonField' = "3"
            demoEvent = Removal 1.0 demoPerson
            demoRecord = toRecord demoEvent
            demoRecord' = V.fromList ["removal", "1.0", "3", "NA"] :: Record
            (Right demoEvent') =
              runParser (parseRecord demoRecord) :: Either String EpidemicEvent
            demoRecord2 =
              toRecord (Catastrophe 1.0 (asPeople [p2, p3]))
            (Right demoEvent2@(Catastrophe _ people2)) =
              runParser (parseRecord demoRecord2) :: Either String EpidemicEvent
            demoRecord2' = toRecord demoEvent2
         in do (demoPersonField' == demoPersonField) `shouldBe` True
               (demoRecord' == demoRecord) `shouldBe` True
               (demoEvent' == demoEvent) `shouldBe` True
               (demoRecord2' == demoRecord2) `shouldBe` True
               (numPeople people2 == 2) `shouldBe` True


inhomExpTests =
  describe "Test the inhomogeneous exponential variate generator" $
  let rate1 = 2.0
      sF1 = fromJust $ asTimed [(0, rate1)]
      mean1 = 1 / rate1
      var1 = 1 / (rate1 ** 2.0)
      sF2 = fromJust $ asTimed [(0, 1e-10),(1, rate1)]
      mean2 = 1 / rate1 + 1
      var2 = var1
      genAction = MWC.createSystemRandom
   in do it "check we can get a positive variate out" $
           do
             gen <- genAction
             u1 <- MWC.uniform gen :: IO Double
             (u1 > 0) `shouldBe` True
             x1 <- inhomExponential sF1 gen
             (x1 > 0) `shouldBe` True
             (x1 < 100) `shouldBe` True
             True `shouldBe` True
         it "check the mean and variance look sensible" $
           do gen <- genAction
              x <- V.replicateM 20000 (inhomExponential sF1 gen)
              withinNPercent 5 (mean x) mean1 `shouldBe` True
              withinNPercent 5 (variance x) var1 `shouldBe` True
         it "check the mean and variance look sensible with delay" $
           do gen <- genAction
              x <- V.replicateM 20000 (inhomExponential sF2 gen)
              withinNPercent 5 (mean x) mean2 `shouldBe` True
              withinNPercent 5 (variance x) var2 `shouldBe` True


illFormedTreeTest =
  describe "Prevent the simulator returning a broken tree" $ do
  let simDuration = 0.2
      simLambda = 3.2
      simMu = 0.3
      simPsi = 0.3
      simRho = 0.15
      simRhoTime = 2.6
      simOmega = 0.3
      simNu = 0.15
      simNuTime = 3.0
      simParams = (simLambda, simMu, simPsi, [(simRhoTime,simRho)], simOmega, [(simNuTime,simNu)])
      simConfig = BDSCOD.configuration simDuration simParams
    in it "stress testing the observed events function" $
       do
         null (BDSCOD.observedEvents []) `shouldBe` True
         simEvents <- simulation True (fromJust simConfig) BDSCOD.allEvents
         any isReconTreeLeaf simEvents `shouldBe` True
         (length (fromJust $ BDSCOD.observedEvents simEvents) > 1) `shouldBe` True





inhomogeneousBDSTest =
  describe "InhomogeneousBDS module tests" $ do
    it "Check the observedEvents filters out removals" $
      let demoAllEvents = [Infection 0.1 p1 p2
                          ,Sampling 0.2 p1
                          ,Removal 0.3 p3
                          ,Sampling 0.4 p2]
          demoObsEvents = [Infection 0.1 p1 p2
                          ,Sampling 0.2 p1
                          ,Sampling 0.4 p2]
          compObsEvents = InhomBDS.observedEvents demoAllEvents
       in do
        (compObsEvents == demoObsEvents) `shouldBe` True



main :: IO ()
main =
  hspec $ do
    eventHandlingTests
    birthDeathTests
    helperFuncTests
    readwriteTests
    inhomExpTests
    illFormedTreeTest
    inhomogeneousBDSTest
