{-# LANGUAGE RecordWildCards #-}

module Epidemic.InhomogeneousBDS
  ( configuration
  , allEvents
  , observedEvents
  , inhomBDSRates
  ) where

import Epidemic.Types.Population
import Epidemic.Types.Events
import Epidemic.Types.Parameter
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
inhomBDSRates :: Timed Rate     -- ^ birth rate
              -> Rate           -- ^ death rate
              -> Rate           -- ^ sample rate
              -> Maybe InhomBDSRates
inhomBDSRates timedBirthRate@(Timed tBrPairs) deathRate sampleRate
  | all (\x -> 0 < snd x) tBrPairs && deathRate >= 0 && sampleRate >= 0 =
    Just $ InhomBDSRates timedBirthRate deathRate sampleRate
  | otherwise = Nothing

-- | Configuration of a inhomogeneous birth-death-sampling simulation.
--
-- Note that this requires that the timed rates are all positive, if they are
-- not it will return @Nothing@ which can lead to cryptic bugs.
configuration :: AbsoluteTime -- ^ Stopping time of the simulation
              -> ([(AbsoluteTime,Rate)], Rate, Rate) -- ^ Birth, Death and Sampling rates
              -> Maybe (SimulationConfiguration InhomBDSRates InhomBDSPop)
configuration maxTime (tBrPairs, deathRate, sampleRate) =
  let (seedPerson, newId) = newPerson initialIdentifier
      bdsPop = InhomBDSPop (People $ V.singleton seedPerson)
   in do timedBirthRate <- asTimed tBrPairs
         maybeIBDSRates <- inhomBDSRates timedBirthRate deathRate sampleRate
         if maxTime > AbsoluteTime 0
           then Just
                  (SimulationConfiguration maybeIBDSRates bdsPop newId maxTime)
           else Nothing

-- | A random event and the state afterwards
randomEvent ::
     InhomBDSRates -- ^ model parameters
  -> AbsoluteTime  -- ^ the current time
  -> InhomBDSPop   -- ^ the population
  -> Identifier -- ^ current identifier
  -> GenIO         -- ^ PRNG
  -> IO (AbsoluteTime, EpidemicEvent, InhomBDSPop, Identifier)
randomEvent inhomRates@(InhomBDSRates brts dr sr) currTime (InhomBDSPop (people@(People peopleVec))) currId gen =
  let popSize = fromIntegral $ numPeople people :: Double
      -- we need a new step function to account for the population size.
      (Just stepFunction) = asTimed [(t,popSize * fromJust (eventRate inhomRates t)) | t <- allTimes brts]
      eventWeights t = V.fromList [fromJust (cadlagValue brts t), dr, sr]
   in do (Just newEventTime) <- inhomExponential stepFunction currTime gen
         eventIx <- categorical (eventWeights newEventTime) gen
         (selectedPerson, unselectedPeople) <- randomPerson people gen
         return $ case eventIx of
           0 -> ( newEventTime
                , Infection newEventTime selectedPerson birthedPerson
                , InhomBDSPop (addPerson birthedPerson people)
                , newId) where (birthedPerson, newId) = newPerson currId
           1 -> (newEventTime, Removal newEventTime selectedPerson, InhomBDSPop unselectedPeople, currId)
           2 -> (newEventTime, Sampling newEventTime selectedPerson, InhomBDSPop unselectedPeople, currId)
           _ -> error "no birth-death-sampling event selected."

-- | The state of the simulation at the time of the last event prior to the
-- stopping time.
allEvents ::
     InhomBDSRates                            -- ^ model parameters
  -> AbsoluteTime                                     -- ^ stopping time
  -> (AbsoluteTime, [EpidemicEvent], InhomBDSPop, Identifier) -- ^ simulation state
  -> GenIO                                    -- ^ PRNG
  -> IO (AbsoluteTime, [EpidemicEvent], InhomBDSPop, Identifier)
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


-- | Just the observable events from a list of all the events in a simulation.
observedEvents :: [EpidemicEvent] -- ^ All of the simulation events
               -> [EpidemicEvent]
observedEvents [] = []
observedEvents events = sort $ sampleTreeEvents''
  where
    sampleTreeEvents'' =
      sampleTreeEvents . sampleTree $ transmissionTree events (Person initialIdentifier)
