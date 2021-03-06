{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Epidemic.Model.InhomogeneousBDS
  ( configuration
  , randomEvent
  , inhomBDSRates
  , InhomBDSRates(..)
  , InhomBDSPop(..)
  ) where

import Epidemic.Types.Time
  ( AbsoluteTime(..)
  , Timed(..)
  , TimeDelta(..)
  , allTimes
  , asTimed
  , diracDeltaValue
  , nextTime
  , cadlagValue
  , timeAfterDelta
  )
import Control.Monad (liftM)
import Data.Maybe (fromJust, isJust, isNothing)
import qualified Data.Vector as V
import Epidemic
import Epidemic.Types.Events
  ( EpidemicEvent(..)
  , maybeEpidemicTree
  )
import Epidemic.Types.Parameter
import Epidemic.Types.Population
import Epidemic.Types.Observations
import Epidemic.Types.Simulation
  ( SimulationConfiguration(..)
  , SimulationRandEvent(..)
  , SimulationState(..)
  )
import Epidemic.Utility
import System.Random.MWC
import System.Random.MWC.Distributions (categorical, exponential)

data InhomBDSRates =
  InhomBDSRates (Timed Rate) Rate Rate

data InhomBDSPop =
  InhomBDSPop People
  deriving (Show)

instance ModelParameters InhomBDSRates InhomBDSPop where
  rNaught _ (InhomBDSRates timedBirthRate deathRate sampleRate) time =
    let birthRate = cadlagValue timedBirthRate time
     in liftM (/ (deathRate + sampleRate)) birthRate
  eventRate _ (InhomBDSRates timedBirthRate deathRate sampleRate) time =
    let birthRate = cadlagValue timedBirthRate time
     in liftM (+ (deathRate + sampleRate)) birthRate
  birthProb _ (InhomBDSRates timedBirthRate deathRate sampleRate) time =
    liftM (\br -> br / (br + deathRate + sampleRate)) $
    cadlagValue timedBirthRate time

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
  -> ([(AbsoluteTime, Rate)], Rate, Rate) -- ^ Birth, Death and Sampling rates
  -> Maybe (SimulationConfiguration InhomBDSRates InhomBDSPop)
configuration maxTime atLeastCherry (tBrPairs, deathRate, sampleRate) =
  let (seedPerson, newId) = newPerson initialIdentifier
      bdsPop = InhomBDSPop (People $ V.singleton seedPerson)
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
                     Nothing
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
randomEvent' inhomRates@(InhomBDSRates brts dr sr) currTime pop@(InhomBDSPop (people@(People peopleVec))) currId gen =
  let popSize = fromIntegral $ numPeople people :: Double
      eventWeights t = V.fromList [fromJust (cadlagValue brts t), dr, sr]
      -- we need a new step function to account for the population size.
      (Just stepFunction) =
        asTimed
          [ (t, popSize * fromJust (eventRate pop inhomRates t))
          | t <- allTimes brts
          ]
   in do (Just newEventTime) <- inhomExponential stepFunction currTime gen
         eventIx <- categorical (eventWeights newEventTime) gen
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
               , IndividualSample newEventTime selectedPerson True
               , InhomBDSPop unselectedPeople
               , currId)
             _ -> error "no birth-death-sampling event selected."
