{-# LANGUAGE RecordWildCards #-}

module Epidemic.InhomogeneousBD
  ( configuration
  , allEvents
  ) where

import Epidemic.Types
import Control.Monad (liftM)
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import System.Random.MWC
import System.Random.MWC.Distributions (bernoulli, exponential)
import Epidemic
import Epidemic.Utility

data InhomBDRates =
  InhomBDRates (Timed Rate) Rate

instance ModelParameters InhomBDRates where
  rNaught (InhomBDRates timedBirthRate deathRate) time =
    let birthRate = cadlagValue timedBirthRate time
     in liftM (/ deathRate) birthRate
  eventRate (InhomBDRates timedBirthRate deathRate) time =
    let birthRate = cadlagValue timedBirthRate time
     in liftM (+ deathRate) birthRate
  birthProb (InhomBDRates timedBirthRate deathRate) time =
    liftM (\br -> br / (br + deathRate)) $ cadlagValue timedBirthRate time

newtype InhomBDPop =
  InhomBDPop People
  deriving (Show)

instance Population InhomBDPop where
  susceptiblePeople _ = Nothing
  infectiousPeople (InhomBDPop people) = Just people
  removedPeople _ = Nothing
  isInfected (InhomBDPop people) = not $ nullPeople people

-- | Return a BD-process parameters object
inhomBirthDeathRates :: [(Time, Rate)] -- ^ birth rate
                     -> Rate           -- ^ death rate
                     -> Maybe InhomBDRates
inhomBirthDeathRates tBrPairs deathRate
  | all (\x -> 0 < snd x) tBrPairs && deathRate >= 0 = liftM (\tbr -> InhomBDRates tbr deathRate) $ asTimed tBrPairs
  | otherwise = Nothing

-- | Configuration of a inhomogeneous birth-death simulation.
configuration :: Time                     -- ^ Duration of the simulation
              -> ([(Time,Rate)], Rate) -- ^ Birth and Death rates
              -> Maybe (SimulationConfiguration InhomBDRates InhomBDPop)
configuration maxTime (tBrPairs, deathRate) =
  let (seedPerson, newId) = newPerson initialIdentifier
      bdPop = InhomBDPop (People $ V.singleton seedPerson)
   in do maybeIBDRates <- inhomBirthDeathRates tBrPairs deathRate
         if maxTime > 0
           then Just (SimulationConfiguration maybeIBDRates bdPop newId maxTime)
           else Nothing

-- | A random event and the state afterwards
randomEvent ::
     InhomBDRates -- ^ model parameters
  -> Time         -- ^ the current time
  -> InhomBDPop   -- ^ the population
  -> Identifier   -- ^ current identifier
  -> GenIO        -- ^ PRNG
  -> IO (Time, Event, InhomBDPop, Identifier)
randomEvent inhomRates@(InhomBDRates brts@(Timed brts') dr) currTime (InhomBDPop (people@(People peopleVec))) currId gen =
  let popSize = fromIntegral $ numPeople people :: Double
      stepTimes = map fst brts'
      stepFunction = fromJust $ asTimed [(t-currTime,popSize * fromJust (eventRate inhomRates t)) | t <- stepTimes]
   in do delay <- inhomExponential stepFunction gen
         isBirth <- bernoulli (fromJust (birthProb inhomRates (currTime + delay))) gen
         (selectedPerson, unselectedPeople) <- randomPerson peopleVec gen
         return $
           if isBirth
             then let newTime = currTime + delay
                      (birthedPerson, newId) = newPerson currId
                      event =
                        InfectionEvent newTime selectedPerson birthedPerson
                   in ( newTime
                      , event
                      , InhomBDPop (addPerson birthedPerson people)
                      , newId)
             else let newTime = currTime + delay
                      event = RemovalEvent newTime selectedPerson
                   in ( newTime
                      , event
                      , InhomBDPop (People unselectedPeople)
                      , currId)

-- | The state of the simulation at the time of the last event prior to the
-- stopping time.
allEvents ::
     InhomBDRates                            -- ^ model parameters
  -> Time                                    -- ^ stopping time
  -> (Time, [Event], InhomBDPop, Identifier) -- ^ simulation state
  -> GenIO                                   -- ^ PRNG
  -> IO (Time, [Event], InhomBDPop, Identifier)
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

