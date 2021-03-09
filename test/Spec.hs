{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (evaluate)
import Control.Monad
import qualified Data.Aeson as Json
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BBuilder
import Data.Either (isRight)
import Data.Maybe (fromJust, isJust, isNothing)
import qualified Data.Vector as V
import Epidemic
import qualified Epidemic.Model.BDSCOD as BDSCOD
import qualified Epidemic.Model.InhomogeneousBDS as InhomBDS
import Epidemic.Types.Events
import Epidemic.Types.Observations
import Epidemic.Types.Time
import Epidemic.Types.Newick
import Epidemic.Types.Parameter
import Epidemic.Types.Population
import Epidemic.Utility
import Statistics.Sample
import qualified System.Random.MWC as MWC
import Test.Hspec

-- | Helper function for converting from Either to Maybe monad.
either2Maybe x = case x of
  Right v -> Just v
  Left _ -> Nothing

-- | y is within n% of x from x.
withinNPercent n x y = x - d < y && y < x + d
  where
    d = n * x / 100

p1 = Person (Identifier 1)

p2 = Person (Identifier 2)

p3 = Person (Identifier 3)

p4 = Person (Identifier 4)

p5 = Person (Identifier 5)

p6 = Person (Identifier 6)

p7 = Person (Identifier 7)

-- | The first set of test data does not have any catastrophe events.
demoFullEvents01 =
  [ Infection (AbsoluteTime 1) p1 p2
  , Infection (AbsoluteTime 2) p1 p3
  , IndividualSample (AbsoluteTime 3) p1 True
  , Infection (AbsoluteTime 4) p2 p4
  , Infection (AbsoluteTime 5) p2 p5
  , IndividualSample (AbsoluteTime 6) p4 True
  , Infection (AbsoluteTime 7) p3 p6
  , IndividualSample (AbsoluteTime 8) p2 False
  , Removal (AbsoluteTime 9) p3
  , Infection (AbsoluteTime 10) p5 p7
  , IndividualSample (AbsoluteTime 11) p6 False
  , IndividualSample (AbsoluteTime 12) p5 True
  , Removal (AbsoluteTime 13) p7
  ]

demoSampleEvents01 =
  [ Infection (AbsoluteTime 1) p1 p2
  , IndividualSample (AbsoluteTime 3) p1 True
  , Infection (AbsoluteTime 4) p2 p4
  , IndividualSample (AbsoluteTime 6) p4 True
  , IndividualSample (AbsoluteTime 8) p2 False
  , IndividualSample (AbsoluteTime 11) p6 False
  , IndividualSample (AbsoluteTime 12) p5 True
  ]

-- | The second set of test data is the same as the first but includes a
-- catastrophe event.
demoFullEvents02 =
  [ Infection (AbsoluteTime 1) p1 p2
  , Infection (AbsoluteTime 2) p1 p3
  , IndividualSample (AbsoluteTime 3) p1 True
  , Infection (AbsoluteTime 4) p2 p4
  , Infection (AbsoluteTime 5) p2 p5
  , IndividualSample (AbsoluteTime 6) p4 True
  , Infection (AbsoluteTime 7) p3 p6
  , IndividualSample (AbsoluteTime 8) p2 False
  , Removal (AbsoluteTime 9) p3
  , Infection (AbsoluteTime 10) p5 p7
  , PopulationSample (AbsoluteTime 11) (asPeople [p5]) True
  , IndividualSample (AbsoluteTime 12) p6 False
  , Removal (AbsoluteTime 13) p7
  ]

demoSampleEvents02 =
  [ Infection (AbsoluteTime 1) p1 p2
  , IndividualSample (AbsoluteTime 3) p1 True
  , Infection (AbsoluteTime 4) p2 p4
  , IndividualSample (AbsoluteTime 6) p4 True
  , IndividualSample (AbsoluteTime 8) p2 False
  , PopulationSample (AbsoluteTime 11) (asPeople [p5]) True
  , IndividualSample (AbsoluteTime 12) p6 False
  ]

-- | Another test set to test that catastrophes are handled correctly.
demoFullEvents03 =
  [ Infection (AbsoluteTime 1) p1 p4
  , Infection (AbsoluteTime 2) p1 p2
  , IndividualSample (AbsoluteTime 3) p1 True
  , Infection (AbsoluteTime 4) p2 p3
  , Infection (AbsoluteTime 5) p4 p5
  , PopulationSample (AbsoluteTime 6) (asPeople [p2, p3, p4]) True
  ]

demoSampleEvents03 =
  [ Infection (AbsoluteTime 1) p1 p4
  , Infection (AbsoluteTime 2) p1 p2
  , IndividualSample (AbsoluteTime 3) p1 True
  , Infection (AbsoluteTime 4) p2 p3
  , PopulationSample (AbsoluteTime 6) (asPeople [p2, p3, p4]) True
  ]

-- | Another test to make sure that disasters are handled.
demoFullEvents04 =
  [ Infection (AbsoluteTime 1) p1 p4
  , Infection (AbsoluteTime 2) p1 p2
  , IndividualSample (AbsoluteTime 3) p1 True
  , Infection (AbsoluteTime 4) p2 p3
  , Infection (AbsoluteTime 5) p4 p5
  , PopulationSample (AbsoluteTime 6) (asPeople [p2, p3, p4]) True
  , Infection (AbsoluteTime 7) p5 p6
  , Infection (AbsoluteTime 8) p5 p7
  , PopulationSample (AbsoluteTime 9) (asPeople [p5, p6]) False
  ]

demoSampleEvents04 =
  [ Infection (AbsoluteTime 1) p1 p4
  , Infection (AbsoluteTime 2) p1 p2
  , IndividualSample (AbsoluteTime 3) p1 True
  , Infection (AbsoluteTime 4) p2 p3
  , PopulationSample (AbsoluteTime 6) (asPeople [p2, p3, p4]) True
  , PopulationSample (AbsoluteTime 9) (asPeople [p5, p6]) False
  ]

eventHandlingTests = do
  describe "Post-simulation processing" $ do
    it "Extracting observed events" $ do
      let demoEvents =
            [ PopulationSample (AbsoluteTime 0.5) (asPeople []) True -- Because the first event is a null event it can be ignored!
            , Infection (AbsoluteTime 1.0) p1 p2
            , PopulationSample (AbsoluteTime 1.5) (asPeople []) True
            , PopulationSample (AbsoluteTime 2.0) (asPeople [p1, p2]) True
            ]
      (length demoEvents == 4) `shouldBe` True
      ((length <$> observedEvents (tail demoEvents)) == (Right 2)) `shouldBe`
        True
      ((length <$> observedEvents (demoEvents)) == (Right 2)) `shouldBe`
        True
      (observedEvents (demoEvents) ==
       observedEvents (tail demoEvents)) `shouldBe`
        True
      (maybeEpidemicTree (demoEvents) == maybeEpidemicTree (tail demoEvents)) `shouldBe`
        True
  describe "Catastrophe definitions" $ do
    it "Check we can find a catastrophe" $ do
      (noScheduledEvent (AbsoluteTime 0) (AbsoluteTime 1) (Timed [])) `shouldBe`
        True
      (noScheduledEvent
         (AbsoluteTime 0)
         (AbsoluteTime 1)
         (Timed [(AbsoluteTime 2, 0.5)])) `shouldBe`
        True
      (noScheduledEvent
         (AbsoluteTime 0)
         (AbsoluteTime 1)
         (Timed [(AbsoluteTime 0.5, 0.5)])) `shouldBe`
        False
      (noScheduledEvent
         (AbsoluteTime 0)
         (AbsoluteTime 1)
         (Timed [(AbsoluteTime 2, 0.6), (AbsoluteTime 0.5, 0.5)])) `shouldBe`
        False
    it "Check we can find a particular catastrophe" $ do
      (firstScheduled (AbsoluteTime 1) (Timed [])) `shouldBe` Nothing
      (firstScheduled (AbsoluteTime 1) (Timed [(AbsoluteTime 2, 0.5)])) `shouldBe`
        Just (AbsoluteTime 2, 0.5)
      (firstScheduled (AbsoluteTime 1) (Timed [(AbsoluteTime 0.5, 0.5)])) `shouldBe`
        Nothing
      (firstScheduled
         (AbsoluteTime 1)
         (Timed [(AbsoluteTime 2, 0.6), (AbsoluteTime 0.5, 0.5)])) `shouldBe`
        Just (AbsoluteTime 2, 0.6)
      isNothing
        (asTimed
           [ (AbsoluteTime 2, 0.6 :: Rate)
           , (AbsoluteTime 0.5, 0.5)
           , (AbsoluteTime 1.5, 0.4)
           ]) `shouldBe`
        True
      (firstScheduled
         (AbsoluteTime 1)
         (Timed
            [ (AbsoluteTime 2, 0.6)
            , (AbsoluteTime 0.5, 0.5)
            , (AbsoluteTime 1.5, 0.4)
            ])) `shouldBe`
        Just (AbsoluteTime 2, 0.6)
    it "Works on a very specific case it seems to not like" $ do
      (noScheduledEvent
         (AbsoluteTime 2.28)
         (AbsoluteTime (2.28 + 0.42))
         (Timed [(AbsoluteTime 2.3, 0.9)])) `shouldBe`
        False
  describe "Disaster definitions" $ do
    it "Disasters are handled correctly" $ do
      ((Right $ [Observation e | e <- demoSampleEvents04]) == (observedEvents demoFullEvents04)) `shouldBe`
        True
    it "Disasters can be simulated" $ do
      demoSim <-
        simulation
          False
          (fromJust
             (BDSCOD.configuration
                (TimeDelta 4)
                ( 1.3
                , 0.1
                , 0.1
                , [(AbsoluteTime 3, 0.5)]
                , 0.2
                , [(AbsoluteTime 3.5, 0.5)])))
          (allEvents BDSCOD.randomEvent)
      length demoSim > 1 `shouldBe` True
  describe "Extracting observed events" $ do
    it "unseq obs still extracted if no seq obs" $ do
      let noSequencedEvents =
            [ Infection (AbsoluteTime 4.1) p1 p2
            , Infection (AbsoluteTime 4.3) p2 p3
            , Infection (AbsoluteTime 4.5) p2 p4
            , IndividualSample
                { indSampTime = AbsoluteTime 5.3
                , indSampPerson = p1
                , indSampSeq = False
                }
            , StoppingTime
            ]
          expectedObs =
            [ Observation
                (IndividualSample
                   { indSampTime = AbsoluteTime 5.3
                   , indSampPerson = p1
                   , indSampSeq = False
                   })
            ]
      isRight (observedEvents noSequencedEvents) `shouldBe` True
      ((Right expectedObs) == (observedEvents noSequencedEvents)) `shouldBe`
        True

helperFuncTests = do
  describe "Helpers in Utility" $ do
    it "the isAscending function works" $ do
      (isAscending ([] :: [AbsoluteTime])) `shouldBe` True
      (isAscending [-1.0]) `shouldBe` True
      (isAscending [1.0]) `shouldBe` True
      (isAscending [1.0, 2.0]) `shouldBe` True
      (isAscending [1.0, 2.0, 3.0]) `shouldBe` True
      (isAscending [1.0, -2.0]) `shouldBe` False
      (isAscending [1.0, -2.0, 3.0]) `shouldBe` False
      (isAscending [1.0, 2.0, -3.0]) `shouldBe` False
    it "the asTimed function works" $ do
      (isJust $ asTimed []) `shouldBe` True
      (isJust $ asTimed [(AbsoluteTime 0, 1)]) `shouldBe` True
      (isJust $ asTimed [(AbsoluteTime 0, 1), (AbsoluteTime 1, 3)]) `shouldBe`
        True
      (isJust $ asTimed [(AbsoluteTime 0, 3), (AbsoluteTime 1, 1)]) `shouldBe`
        True
      (isJust $ asTimed [(AbsoluteTime 1, 3), (AbsoluteTime 0, 1)]) `shouldBe`
        False
    let demoTimed =
          fromJust $
          asTimed
            [ (AbsoluteTime 0, 1.2)
            , (AbsoluteTime 1, 3.1)
            , (AbsoluteTime 2, 2.7)
            ]
     in do it "the cadlagValue function works" $ do
             (isJust $ cadlagValue demoTimed (AbsoluteTime (-1.0))) `shouldBe`
               False
             ((== 1.2) . fromJust $ cadlagValue demoTimed (AbsoluteTime 0.0)) `shouldBe`
               True
             ((== 1.2) . fromJust $ cadlagValue demoTimed (AbsoluteTime 0.5)) `shouldBe`
               True
             ((== 3.1) . fromJust $ cadlagValue demoTimed (AbsoluteTime 1.5)) `shouldBe`
               True
           it "the diracDeltaValue function works" $ do
             ((== 1.2) . fromJust $ diracDeltaValue demoTimed (AbsoluteTime 0)) `shouldBe`
               True
             (isJust $ diracDeltaValue demoTimed (AbsoluteTime 1)) `shouldBe`
               True
             (isJust $ diracDeltaValue demoTimed (AbsoluteTime 0.9)) `shouldBe`
               False
             (isJust $ diracDeltaValue demoTimed (AbsoluteTime 1.1)) `shouldBe`
               False
           it "the hasTime function works" $ do
             (hasTime demoTimed (AbsoluteTime 0)) `shouldBe` True
             (hasTime demoTimed (AbsoluteTime 0.5)) `shouldBe` False
             (hasTime demoTimed (AbsoluteTime 1)) `shouldBe` True
             (hasTime demoTimed (AbsoluteTime 1.5)) `shouldBe` False
           it "the nextTime function works" $ do
             (AbsoluteTime 0 ==
              (fromJust $ nextTime demoTimed (AbsoluteTime (-1)))) `shouldBe`
               True
             (AbsoluteTime 1 == (fromJust $ nextTime demoTimed (AbsoluteTime 0))) `shouldBe`
               True
             (AbsoluteTime 1 ==
              (fromJust $ nextTime demoTimed (AbsoluteTime 0.5))) `shouldBe`
               True
           it "the nextTime function handles the last time correctly" $ do
             isJust (nextTime demoTimed (AbsoluteTime 1.9)) `shouldBe` True
             isJust (nextTime demoTimed (AbsoluteTime 2.0)) `shouldBe` True
             isJust (nextTime demoTimed (AbsoluteTime 2.1)) `shouldBe` True
             isJust (nextTime demoTimed (AbsoluteTime 10.0)) `shouldBe` True
    it "shifted times work" $
      let sf =
            fromJust $
            asTimed [(AbsoluteTime (-1.0), 2.0), (AbsoluteTime 1, 3.0)]
          val1 = cadlagValue sf (AbsoluteTime 0)
          val2 = cadlagValue sf (AbsoluteTime (-2.0))
          val3 = cadlagValue sf (AbsoluteTime 1.5)
       in do isJust val1 `shouldBe` True
             val1 == Just 2.0 `shouldBe` True
             (not $ isJust val2) `shouldBe` True
             isJust val3 `shouldBe` True
             val3 == Just 3.0 `shouldBe` True
    it "the asTimed function returns nothing as expected" $ do
      (isJust $ asTimed [(AbsoluteTime 0.0, -1)]) `shouldBe` True
      (isJust $ asTimed [(AbsoluteTime 0.0, 1), (AbsoluteTime 1.0, -1)]) `shouldBe`
        True
      let (Just timedBirthRate) =
            asTimed [(AbsoluteTime 0.0, 1.0), (AbsoluteTime 1.0, -1.0)]
      (isJust $ InhomBDS.inhomBDSRates timedBirthRate 0.5 0.5) `shouldBe` False

inhomExpTests =
  describe "Test the inhomogeneous exponential variate generator" $
  let rate1 = 2.0
      sF1 = fromJust $ asTimed [(AbsoluteTime 0, rate1)]
      mean1 = 1 / rate1
      var1 = 1 / (rate1 ** 2.0)
      sF2 =
        fromJust $ asTimed [(AbsoluteTime 0, 1e-10), (AbsoluteTime 1, rate1)]
      mean2 = 1 / rate1 + 1
      var2 = var1
      genAction = MWC.createSystemRandom
   in do it "check we can get a positive variate out" $ do
           gen <- genAction
           u1 <- MWC.uniform gen :: IO Double
           (u1 > 0) `shouldBe` True
           (Just x1) <- inhomExponential sF1 (AbsoluteTime 0) gen
           (x1 > AbsoluteTime 0) `shouldBe` True
           (x1 < AbsoluteTime 100) `shouldBe` True
           True `shouldBe` True
         it "check the mean and variance look sensible" $ do
           gen <- genAction
           xBoxed <-
             V.replicateM 20000 (inhomExponential sF1 (AbsoluteTime 0) gen)
           let x = fmap (\(Just (AbsoluteTime t)) -> t) xBoxed
           withinNPercent 5 (mean x) mean1 `shouldBe` True
           withinNPercent 5 (variance x) var1 `shouldBe` True
         it "check the mean and variance look sensible with delay" $ do
           gen <- genAction
           xBoxed <-
             V.replicateM 20000 (inhomExponential sF2 (AbsoluteTime 0) gen)
           let x = fmap (\(Just (AbsoluteTime t)) -> t) xBoxed
           withinNPercent 5 (mean x) mean2 `shouldBe` True
           withinNPercent 5 (variance x) var2 `shouldBe` True

illFormedTreeTest :: SpecWith ()
illFormedTreeTest =
  describe "Prevent the simulator returning a broken tree" $ do
    let simDuration = TimeDelta 0.2
        simLambda = 3.2
        simMu = 0.3
        simPsi = 0.3
        simRho = 0.15
        simRhoTime = AbsoluteTime 2.6
        simOmega = 0.3
        simNu = 0.15
        simNuTime = AbsoluteTime 3.0
        simParams =
          ( simLambda
          , simMu
          , simPsi
          , [(simRhoTime, simRho)]
          , simOmega
          , [(simNuTime, simNu)])
        simConfig = BDSCOD.configuration simDuration simParams
     in it "stress testing the observed events function" $ do
          null (observedEvents []) `shouldBe` True
          simEvents <-
            simulation True (fromJust simConfig) (allEvents BDSCOD.randomEvent)
          any isReconTreeLeaf simEvents `shouldBe` True
          let (Right oes) = observedEvents simEvents
          (length oes > 1) `shouldBe` True

inhomogeneousBDSTest =
  describe "InhomogeneousBDS module tests" $ do
    it "Check the observedEvents filters out removals" $
      let demoAllEvents =
            [ Infection (AbsoluteTime 0.1) p1 p2
            , IndividualSample (AbsoluteTime 0.2) p1 True
            , Removal (AbsoluteTime 0.3) p3
            , IndividualSample (AbsoluteTime 0.4) p2 True
            ]
          demoObsEvents =
            [ Infection (AbsoluteTime 0.1) p1 p2
            , IndividualSample (AbsoluteTime 0.2) p1 True
            , IndividualSample (AbsoluteTime 0.4) p2 True
            ]
          compObsEvents = observedEvents demoAllEvents
       in do (compObsEvents == (Right [Observation e | e <- demoObsEvents])) `shouldBe` True

helperTypeTests = do
  describe "Helpers for working with the types" $ do
    it "the isAscending function works" $ do
      (isAscending ([] :: [AbsoluteTime])) `shouldBe` True
      (isAscending [-1.0]) `shouldBe` True
      (isAscending [1.0]) `shouldBe` True
      (isAscending [1.0, 2.0]) `shouldBe` True
      (isAscending [1.0, 2.0, 3.0]) `shouldBe` True
      (isAscending [1.0, -2.0]) `shouldBe` False
      (isAscending [1.0, -2.0, 3.0]) `shouldBe` False
      (isAscending [1.0, 2.0, -3.0]) `shouldBe` False
    it "the asTimed function works" $ do
      (isJust $ asTimed []) `shouldBe` True
      (isJust $ asTimed [(AbsoluteTime 0, 1)]) `shouldBe` True
      (isJust $ asTimed [(AbsoluteTime 0, 1), (AbsoluteTime 1, 3)]) `shouldBe`
        True
      (isJust $ asTimed [(AbsoluteTime 0, 3), (AbsoluteTime 1, 1)]) `shouldBe`
        True
      (isJust $ asTimed [(AbsoluteTime 1, 3), (AbsoluteTime 0, 1)]) `shouldBe`
        False
    let demoTimed =
          fromJust $
          asTimed
            [ (AbsoluteTime 0, 1.2)
            , (AbsoluteTime 1, 3.1)
            , (AbsoluteTime 2, 2.7)
            ]
     in do it "the cadlagValue function works" $ do
             (isJust $ cadlagValue demoTimed (AbsoluteTime (-1.0))) `shouldBe`
               False
             ((== 1.2) . fromJust $ cadlagValue demoTimed (AbsoluteTime 0.0)) `shouldBe`
               True
             ((== 1.2) . fromJust $ cadlagValue demoTimed (AbsoluteTime 0.5)) `shouldBe`
               True
             ((== 3.1) . fromJust $ cadlagValue demoTimed (AbsoluteTime 1.5)) `shouldBe`
               True
           it "the diracDeltaValue function works" $ do
             ((== 1.2) . fromJust $ diracDeltaValue demoTimed (AbsoluteTime 0)) `shouldBe`
               True
             (isJust $ diracDeltaValue demoTimed (AbsoluteTime 1)) `shouldBe`
               True
             (isJust $ diracDeltaValue demoTimed (AbsoluteTime 0.9)) `shouldBe`
               False
             (isJust $ diracDeltaValue demoTimed (AbsoluteTime 1.1)) `shouldBe`
               False
           it "the hasTime function works" $ do
             (hasTime demoTimed (AbsoluteTime 0)) `shouldBe` True
             (hasTime demoTimed (AbsoluteTime 0.5)) `shouldBe` False
             (hasTime demoTimed (AbsoluteTime 1)) `shouldBe` True
             (hasTime demoTimed (AbsoluteTime 1.5)) `shouldBe` False
           it "the nextTime function works" $ do
             (AbsoluteTime 0 ==
              (fromJust $ nextTime demoTimed (AbsoluteTime (-1)))) `shouldBe`
               True
             (AbsoluteTime 1 == (fromJust $ nextTime demoTimed (AbsoluteTime 0))) `shouldBe`
               True
             (AbsoluteTime 1 ==
              (fromJust $ nextTime demoTimed (AbsoluteTime 0.5))) `shouldBe`
               True
    it "shifted times work" $
      let sf =
            fromJust $
            asTimed [(AbsoluteTime (-1.0), 2.0), (AbsoluteTime 1, 3.0)]
          val1 = cadlagValue sf (AbsoluteTime 0)
          val2 = cadlagValue sf (AbsoluteTime (-2.0))
          val3 = cadlagValue sf (AbsoluteTime 1.5)
       in do isJust val1 `shouldBe` True
             val1 == Just 2.0 `shouldBe` True
             (not $ isJust val2) `shouldBe` True
             isJust val3 `shouldBe` True
             val3 == Just 3.0 `shouldBe` True

jsonTests = do
  describe "Converting to and from JSON" $ do
    it "Conversion of Timed Rate" $ do
      let demoObj =
            Timed [(AbsoluteTime 0.0, 1.0), (AbsoluteTime 1.0, 1.0)] :: Timed Rate
          (Timed demoVals) = demoObj
          demoJson = "[[0,1],[1,1]]"
          encodedObj = Json.encode demoObj
          decodedJson = Json.decode demoJson :: Maybe (Timed Rate)
       in do True `shouldBe` True
             let (Timed foo) = demoObj
              in demoVals == foo `shouldBe` True
             encodedObj == demoJson `shouldBe` True
             isJust decodedJson `shouldBe` True
             let (Timed bar) = fromJust decodedJson
              in demoVals == bar `shouldBe` True

equalBuilders :: BBuilder.Builder -> BBuilder.Builder -> Bool
equalBuilders a b = BBuilder.toLazyByteString a == BBuilder.toLazyByteString b

newickTests =
  let p1 = Person (Identifier 1)
      p2 = Person (Identifier 2)
      p3 = Person (Identifier 3)
      ps = asPeople [p1, p2]
      maybeEpiTree =
        maybeEpidemicTree
          [ Infection (AbsoluteTime 1) p1 p2
          , Infection (AbsoluteTime 2) p2 p3
          , PopulationSample (AbsoluteTime 3) (asPeople [p1, p3]) True
          , Removal (AbsoluteTime 4) p2
          ]
      maybeEpiTree' =
        maybeEpidemicTree
          [ Infection (AbsoluteTime 1) p1 p2
          , Infection (AbsoluteTime 2) p2 p3
          , PopulationSample (AbsoluteTime 3) (asPeople [p1, p3]) True
          , IndividualSample (AbsoluteTime 4) p2 True
          ]
      maybeEpiTree'' =
        maybeEpidemicTree
          [ Infection (AbsoluteTime 1) p1 p2
          , Infection (AbsoluteTime 2) p2 p3
          , PopulationSample (AbsoluteTime 3) (asPeople [p1, p3]) False
          , IndividualSample (AbsoluteTime 4) p2 True
          ]
   in describe "Writing to Newick" $ do
        it "equalBuilders works as expected" $ do
          equalBuilders (BBuilder.charUtf8 ':') (BBuilder.charUtf8 ':') `shouldBe`
            True
          equalBuilders (BBuilder.charUtf8 'a') (BBuilder.charUtf8 ':') `shouldBe`
            False
        it "derivedFrom works as expected" $ do
          let p1 = Person (Identifier 1)
          let p2 = Person (Identifier 2)
          let p3 = Person (Identifier 3)
          let e = [Infection (AbsoluteTime 0.3) p1 p2]
          derivedFrom p1 e == derivedFrom p2 e `shouldBe` True
          derivedFrom p1 e /= derivedFrom p3 e `shouldBe` True
          derivedFrom p1 e /= [] `shouldBe` True
          null (derivedFrom p3 e) `shouldBe` True
          derivedFrom p1 e == e `shouldBe` True
          let foo =
                derivedFrom
                  (Person (Identifier 1))
                  [ Infection
                      (AbsoluteTime 0.3)
                      (Person (Identifier 1))
                      (Person (Identifier 2))
                  , IndividualSample
                      (AbsoluteTime 0.7)
                      (Person (Identifier 1))
                      True
                  ]
          let bar =
                derivedFrom
                  (Person (Identifier 2))
                  [ Infection
                      (AbsoluteTime 0.3)
                      (Person (Identifier 1))
                      (Person (Identifier 2))
                  , IndividualSample
                      (AbsoluteTime 0.7)
                      (Person (Identifier 1))
                      True
                  ]
          foo == bar `shouldBe` True
        it "maybeEpidemicTree works as expected: 1" $ do
          let e1 = Removal (AbsoluteTime 1) (Person (Identifier 1))
          maybeEpidemicTree [e1] == Right (Leaf e1) `shouldBe` True
          let t1 =
                maybeEpidemicTree
                  [ Infection
                      (AbsoluteTime 0.3)
                      (Person (Identifier 1))
                      (Person (Identifier 2))
                  , IndividualSample
                      (AbsoluteTime 0.6)
                      (Person (Identifier 2))
                      True
                  , IndividualSample
                      (AbsoluteTime 0.7)
                      (Person (Identifier 1))
                      True
                  ]
          let t2 =
                Right
                  (Branch
                     (Infection
                        (AbsoluteTime 0.3)
                        (Person (Identifier 1))
                        (Person (Identifier 2)))
                     (Leaf
                        (IndividualSample
                           (AbsoluteTime 0.7)
                           (Person (Identifier 1))
                           True))
                     (Leaf
                        (IndividualSample
                           (AbsoluteTime 0.6)
                           (Person (Identifier 2))
                           True)))
          t1 == t2 `shouldBe` True
          maybeEpidemicTree
            [ Infection
                (AbsoluteTime 0.3)
                (Person (Identifier 1))
                (Person (Identifier 2))
            ] ==
            Right
              (Branch
                 (Infection
                    (AbsoluteTime 0.3)
                    (Person (Identifier 1))
                    (Person (Identifier 2)))
                 (Shoot (Person (Identifier 1)))
                 (Shoot (Person (Identifier 2)))) `shouldBe`
            True
          maybeEpidemicTree
            [ Infection
                (AbsoluteTime 0.3)
                (Person (Identifier 1))
                (Person (Identifier 2))
            , IndividualSample (AbsoluteTime 0.7) (Person (Identifier 1)) True
            ] ==
            Right
              (Branch
                 (Infection
                    (AbsoluteTime 0.3)
                    (Person (Identifier 1))
                    (Person (Identifier 2)))
                 (Leaf
                    (IndividualSample
                       (AbsoluteTime 0.7)
                       (Person (Identifier 1))
                       True))
                 (Shoot (Person (Identifier 2)))) `shouldBe`
            True
          let trickyEvents =
                [ Infection
                    (AbsoluteTime 0.3)
                    (Person (Identifier 1))
                    (Person (Identifier 2))
                , Infection
                    (AbsoluteTime 0.4)
                    (Person (Identifier 2))
                    (Person (Identifier 3))
                , IndividualSample
                    (AbsoluteTime 0.6)
                    (Person (Identifier 3))
                    True
                , IndividualSample
                    (AbsoluteTime 0.7)
                    (Person (Identifier 1))
                    True
                ]
          isRight (maybeEpidemicTree trickyEvents) `shouldBe` True
        it "maybeEpidemicTree works as expected: 2" $ do
          let p1 = Person (Identifier 1)
              p2 = Person (Identifier 2)
              demoEvents =
                [ PopulationSample (AbsoluteTime 0.5) (asPeople []) True -- Because the first event is a null event it can be ignored!
                , Infection (AbsoluteTime 1.0) p1 p2
                , PopulationSample (AbsoluteTime 1.5) (asPeople []) True
                , PopulationSample (AbsoluteTime 2.0) (asPeople [p1, p2]) True
                ]
          (length demoEvents == 4) `shouldBe` True
          (maybeEpidemicTree demoEvents == maybeEpidemicTree (tail demoEvents)) `shouldBe`
            True
    -- it "asNewickString works for EpidemicTree" $ do
    --   let trickyEvents = [
    --         Infection (AbsoluteTime 0.3) (Person (Identifier 1)) (Person (Identifier 2)),
    --         Infection (AbsoluteTime 0.4) (Person (Identifier 2)) (Person (Identifier 3)),
    --         IndividualSample (AbsoluteTime 0.6) (Person (Identifier 3)) True,
    --         IndividualSample (AbsoluteTime 0.7) (Person (Identifier 1)) True]
    --   let maybeNewickPair = asNewickString (AbsoluteTime 0, Person (Identifier 1)) =<< maybeEpidemicTree trickyEvents
    --   let newickTarget = BBuilder.stringUtf8 "(1:0.39999999999999997,(2:Infinity,3:0.19999999999999996):0.10000000000000003):0.3"
    --   let maybeReconTree = maybeReconstructedTree =<< maybeEpidemicTree trickyEvents
    --   isJust maybeNewickPair `shouldBe` True
    --   [IndividualSample (AbsoluteTime 0.6) (Person (Identifier 3)) True, IndividualSample (AbsoluteTime 0.7) (Person (Identifier 1)) True] == snd (fromJust maybeNewickPair) `shouldBe` True
    --   equalBuilders newickTarget (fst $ fromJust maybeNewickPair) `shouldBe` True
    --   isJust maybeReconTree `shouldBe` True
        it "asNewickString works for ReconstructedTree" $ do
          isJust
            (asNewickString
               (AbsoluteTime 0, Person (Identifier 1))
               (RLeaf
                  (Observation (IndividualSample
                     (AbsoluteTime 1)
                     (Person (Identifier 1))
                     True)))) `shouldBe`
            True
          let trickyEvents =
                [ Infection
                    (AbsoluteTime 0.3)
                    (Person (Identifier 1))
                    (Person (Identifier 2))
                , Infection
                    (AbsoluteTime 0.4)
                    (Person (Identifier 2))
                    (Person (Identifier 3))
                , IndividualSample
                    (AbsoluteTime 0.6)
                    (Person (Identifier 3))
                    True
                , IndividualSample
                    (AbsoluteTime 0.7)
                    (Person (Identifier 1))
                    True
                ]
          let et = maybeEpidemicTree trickyEvents :: Either String EpidemicTree
              rt = maybeReconstructedTree =<< et :: Either String ReconstructedTree
              maybeNewickPair = asNewickString (AbsoluteTime 0, Person (Identifier 1)) =<< (either2Maybe rt)
          let newickTarget =
                BBuilder.stringUtf8 "(1:0.39999999999999997,3:0.3):0.3"
          isJust maybeNewickPair `shouldBe` True
          [ IndividualSample (AbsoluteTime 0.6) (Person (Identifier 3)) True, IndividualSample (AbsoluteTime 0.7) (Person (Identifier 1)) True] == snd (fromJust maybeNewickPair) `shouldBe` True
          equalBuilders newickTarget (fst $ fromJust maybeNewickPair) `shouldBe`
            True
          let catasNewick =
                (asNewickString
                   (AbsoluteTime 0, Person (Identifier 1))
                   (RLeaf
                      (Observation (PopulationSample
                         (AbsoluteTime 1)
                         (asPeople
                            [Person (Identifier 1), Person (Identifier 2)])
                         True))))
          let catasTarget = BBuilder.stringUtf8 "1&2:1.0"
          equalBuilders catasTarget (fst $ fromJust catasNewick) `shouldBe` True

main :: IO ()
main =
  hspec $ do
    eventHandlingTests
    helperFuncTests
    inhomExpTests
    illFormedTreeTest
    inhomogeneousBDSTest
    helperTypeTests
    jsonTests
    newickTests
