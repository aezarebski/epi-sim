{-# LANGUAGE DeriveGeneric #-}
module Epidemic where

import GHC.Generics (Generic)
import qualified Data.Vector as V

type Time = Double

type Identifier = Integer

type Rate = Double

data Person =
  Person Identifier
  deriving (Show,Generic)

data Event
  = InfectionEvent Time Person Person
  | RemovalEvent Time Person
  deriving (Show,Generic)

class ModelParameters a where
  rNaught :: a -> Double

class Population a where
  susceptiblePeople :: a -> Maybe (V.Vector Person)
  infectiousPeople :: a -> Maybe (V.Vector Person)
  removedPeople :: a -> Maybe (V.Vector Person)
  isInfected :: a -> Bool
