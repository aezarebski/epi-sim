{-# LANGUAGE DeriveGeneric #-}

module Epidemic where

import qualified Data.Vector as V
import GHC.Generics (Generic)

type Time = Double

type Identifier = Integer

type Rate = Double

newtype Person =
  Person Identifier
  deriving (Show, Generic)

newtype People = People (V.Vector Person) deriving (Show)

data Event
  = InfectionEvent Time Person Person -- infection time, infector, infectee
  | RemovalEvent Time Person
  | SamplingEvent Time Person
  | OccurrenceEvent Time Person
  deriving (Show, Generic)

isInfection :: Event -> Bool
isInfection e =
  case e of
    InfectionEvent {} -> True
    _ -> False

class ModelParameters a where
  rNaught :: a -> Double
  eventRate :: a -> Double

class Population a where
  susceptiblePeople :: a -> Maybe People
  infectiousPeople :: a -> Maybe People
  removedPeople :: a -> Maybe People
  isInfected :: a -> Bool
