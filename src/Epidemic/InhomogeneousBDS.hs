{-# LANGUAGE RecordWildCards #-}

module Epidemic.InhomogeneousBDS
  ( configuration
  , allEvents
  , inhomBDSRates
  ) where

import Epidemic.Types
import Control.Monad (liftM)
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import System.Random.MWC
import System.Random.MWC.Distributions (categorical, exponential)
import Epidemic
import Epidemic.Utility

data InhomBDSRates =
  InhomBDSRates (Timed Rate) Rate Rate

instance ModelParameters InhomBDSRates where
  rNaught (InhomBDSRates timedBirthRate deathRate sampleRate) time =
    let birthRate = cadlagValue timedBirthRate time
     in liftM (/ (deathRate + sampleRate)) birthRate
  eventRate (InhomBDSRates timedBirthRate deathRate sampleRate) time =
    let birthRate = cadlagValue timedBirthRate time
     in liftM (+ (deathRate + sampleRate)) birthRate
  birthProb (InhomBDSRates timedBirthRate deathRate sampleRate) time =
    liftM (\br -> br / (br + deathRate + sampleRate)) $ cadlagValue timedBirthRate time

newtype InhomBDSPop =
  InhomBDSPop People
  deriving (Show)

instance Population InhomBDSPop where
  susceptiblePeople _ = Nothing
  infectiousPeople (InhomBDSPop people) = Just people
  removedPeople _ = Nothing
  isInfected (InhomBDSPop people) = not $ nullPeople people

-- | Return a BDS-process parameters object
--
-- Note that this requires that the rates are all positive, if they are not it
-- will return @Nothing@.
inhomBDSRates :: [(Time, Rate)] -- ^ birth rate
              -> Rate           -- ^ death rate
              -> Rate           -- ^ sample rate
              -> Maybe InhomBDSRates
inhomBDSRates tBrPairs deathRate sampleRate
  | all (\x -> 0 < snd x) tBrPairs && deathRate >= 0 && sampleRate >= 0 =
    (\tbr -> InhomBDSRates tbr deathRate sampleRate) <$> asTimed tBrPairs
  | otherwise = Nothing

-- | Configuration of a inhomogeneous birth-death-sampling simulation.
--
-- Note that this requires that the timed rates are all positive, if they are
-- not it will return @Nothing@ which can lead to cryptic bugs.
configuration :: Time                        -- ^ Duration of the simulation
              -> ([(Time,Rate)], Rate, Rate) -- ^ Birth, Death and Sampling rates
              -> Maybe (SimulationConfiguration InhomBDSRates InhomBDSPop)
configuration maxTime (tBrPairs, deathRate, sampleRate) =
  let (seedPerson, newId) = newPerson initialIdentifier
      bdsPop = InhomBDSPop (People $ V.singleton seedPerson)
   in do maybeIBDSRates <- inhomBDSRates tBrPairs deathRate sampleRate
         if maxTime > 0
           then Just
                  (SimulationConfiguration maybeIBDSRates bdsPop newId maxTime)
           else Nothing

-- | A random event and the state afterwards
randomEvent ::
     InhomBDSRates -- ^ model parameters
  -> Time          -- ^ the current time
  -> InhomBDSPop   -- ^ the population
  -> Identifier    -- ^ current identifier
  -> GenIO         -- ^ PRNG
  -> IO (Time, Event, InhomBDSPop, Identifier)
randomEvent inhomRates@(InhomBDSRates brts dr sr) currTime (InhomBDSPop (people@(People peopleVec))) currId gen =
  let popSize = fromIntegral $ numPeople people :: Double
      stepTimes = map fst brts
      stepFunction = fromJust $ asTimed [(t-currTime,popSize * fromJust (eventRate inhomRates t)) | t <- stepTimes]
      eventWeights t = V.fromList [fromJust (cadlagValue brts t), dr, sr]
   in do delay <- inhomExponential stepFunction gen
         eventIx <- categorical (eventWeights (currTime + delay)) gen
         (selectedPerson, unselectedPeople) <- randomPerson peopleVec gen
         return $ case eventIx of
           0 -> let newTime = currTime + delay
                    (birthedPerson, newId) = newPerson currId
                    event = InfectionEvent newTime selectedPerson birthedPerson
                in ( newTime
                   , event
                   , InhomBDSPop (addPerson birthedPerson people)
                   , newId)
           1 -> let newTime = currTime + delay
                    event = RemovalEvent newTime selectedPerson
                in (newTime, event, InhomBDSPop (People unselectedPeople), currId)
           2 -> let newTime = currTime + delay
                    event = SamplingEvent newTime selectedPerson
                in (newTime, event, InhomBDSPop (People unselectedPeople), currId)
           _ -> error "no birth-death-sampling event selected."

-- | The state of the simulation at the time of the last event prior to the
-- stopping time.
allEvents ::
     InhomBDSRates                            -- ^ model parameters
  -> Time                                     -- ^ stopping time
  -> (Time, [Event], InhomBDSPop, Identifier) -- ^ simulation state
  -> GenIO                                    -- ^ PRNG
  -> IO (Time, [Event], InhomBDSPop, Identifier)
allEvents rates maxTime currState@(currTime, currEvents, currPop, currId) gen =
  if isInfected currPop
    then do
      (newTime, event, newPop, newId) <-
        randomEvent rates currTime currPop currId gen
      if newTime < maxTime
        then allEvents
               rates
               maxTime
               (newTime, event : currEvents, newPop, newId)
               gen
        else return currState
    else return currState

