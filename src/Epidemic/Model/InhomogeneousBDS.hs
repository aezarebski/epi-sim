{-# LANGUAGE MultiParamTypeClasses #-}

module Epidemic.Model.InhomogeneousBDS
  ( configuration
  , randomEvent
  , inhomBDSRates
  , InhomBDSRates(..)
  , InhomBDSPop(..)
  ) where

import Epidemic.Data.Time
  ( AbsoluteTime(..)
  , Timed(..)
  , TimeDelta(..)
  , allTimes
  , asTimed
  , cadlagValue
  )
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import Epidemic.Data.Events
  ( EpidemicEvent(..)
  )
import Epidemic.Data.Parameter
import Epidemic.Data.Population
import Epidemic.Data.Simulation
  ( SimulationConfiguration(..)
  , SimulationRandEvent(..), TerminationHandler(..)
  )
import Epidemic.Utility
import System.Random.MWC
import System.Random.MWC.Distributions (categorical)
import qualified Data.Set as Set

data InhomBDSRates =
  InhomBDSRates (Timed Rate) Rate Rate

data InhomBDSPop =
  InhomBDSPop People
  deriving (Show)

instance ModelParameters InhomBDSRates InhomBDSPop where
  rNaught _ (InhomBDSRates timedBirthRate deathRate sampleRate) time =
    let birthRate = cadlagValue timedBirthRate time
     in (/ (deathRate + sampleRate)) <$> birthRate
  eventRate _ (InhomBDSRates timedBirthRate deathRate sampleRate) time =
    let birthRate = cadlagValue timedBirthRate time
     in (+ (deathRate + sampleRate)) <$> birthRate
  birthProb _ (InhomBDSRates timedBirthRate deathRate sampleRate) time =
    (\br -> br / (br + deathRate + sampleRate)) <$>
    cadlagValue timedBirthRate time
  eventWeights _ (InhomBDSRates timedBirthRate deathRate sampleRate) time =
    Just $ V.fromList [fromJust (cadlagValue timedBirthRate time), deathRate, sampleRate]

instance Population InhomBDSPop where
  susceptiblePeople _ = Nothing
  infectiousPeople (InhomBDSPop people) = Just people
  removedPeople _ = Nothing
  isInfected (InhomBDSPop people) = not $ nullPeople people

-- | Return a BDS-process parameters object
--
-- Note that this requires that the rates are all positive, if they are not it
-- will return @Nothing@.
inhomBDSRates ::
     Timed Rate -- ^ birth rate
  -> Rate -- ^ death rate
  -> Rate -- ^ sample rate
  -> Maybe InhomBDSRates
inhomBDSRates timedBirthRate@(Timed tBrPairs) deathRate sampleRate
  | all (\x -> 0 < snd x) tBrPairs && deathRate >= 0 && sampleRate >= 0 =
    Just $ InhomBDSRates timedBirthRate deathRate sampleRate
  | otherwise = Nothing

-- | Configuration of a inhomogeneous birth-death-sampling simulation.
--
-- Note that this requires that the timed rates are all positive, if they are
-- not it will return @Nothing@ which can lead to cryptic bugs.
configuration ::
     TimeDelta -- ^ Duration of the simulation after starting at time 0.
  -> Bool -- ^ condition upon at least two sequenced samples.
  -> Maybe (InhomBDSPop -> Bool, [EpidemicEvent] -> s) -- ^ values for termination handling.
  -> ([(AbsoluteTime, Rate)], Rate, Rate) -- ^ Birth, Death and Sampling rates
  -> Maybe (SimulationConfiguration InhomBDSRates InhomBDSPop s)
configuration maxTime atLeastCherry maybeTHFuncs (tBrPairs, deathRate, sampleRate) =
  let (seedPerson, newId) = newPerson initialIdentifier
      bdsPop = InhomBDSPop (People $ Set.singleton seedPerson)
      termHandler = do (f1, f2) <- maybeTHFuncs
                       return $ TerminationHandler f1 f2
   in do timedBirthRate <- asTimed tBrPairs
         maybeIBDSRates <- inhomBDSRates timedBirthRate deathRate sampleRate
         if maxTime > TimeDelta 0
           then Just
                  (SimulationConfiguration
                     maybeIBDSRates
                     bdsPop
                     newId
                     (AbsoluteTime 0)
                     maxTime
                     termHandler
                     atLeastCherry)
           else Nothing

randomEvent :: SimulationRandEvent InhomBDSRates InhomBDSPop
randomEvent = SimulationRandEvent randomEvent'

-- | A random event and the state afterwards
randomEvent' ::
     InhomBDSRates -- ^ model parameters
  -> AbsoluteTime -- ^ the current time
  -> InhomBDSPop -- ^ the population
  -> Identifier -- ^ current identifier
  -> GenIO -- ^ PRNG
  -> IO (AbsoluteTime, EpidemicEvent, InhomBDSPop, Identifier)
randomEvent' inhomRates@(InhomBDSRates brts _ _) currTime pop@(InhomBDSPop people) currId gen =
  let popSize = fromIntegral $ numPeople people :: Double
      --weightVecFunc :: AbsoluteTime -> Maybe (Vector Double)
      weightVecFunc = eventWeights pop inhomRates
      -- we need a new step function to account for the population size.
      (Just stepFunction) =
        asTimed
          [ (t, popSize * fromJust (eventRate pop inhomRates t))
          | t <- allTimes brts
          ]
   in do (Just newEventTime) <- inhomExponential stepFunction currTime gen
         eventIx <- categorical (fromJust $ weightVecFunc newEventTime) gen
         (selectedPerson, unselectedPeople) <- randomPerson people gen
         return $
           case eventIx of
             0 ->
               ( newEventTime
               , Infection newEventTime selectedPerson birthedPerson
               , InhomBDSPop (addPerson birthedPerson people)
               , newId)
               where (birthedPerson, newId) = newPerson currId
             1 ->
               ( newEventTime
               , Removal newEventTime selectedPerson
               , InhomBDSPop unselectedPeople
               , currId)
             2 ->
               ( newEventTime
               , IndividualSample newEventTime selectedPerson True True
               , InhomBDSPop unselectedPeople
               , currId)
             _ -> error "no birth-death-sampling event selected."
