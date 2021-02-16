{-# LANGUAGE DeriveGeneric #-}

module Epidemic.Types.Observations
  ( Observation(..)
  , observedEvents
  ) where

import qualified Data.Aeson as Json
import Epidemic.Types.Events (EpidemicEvent(..))
import GHC.Generics

-- | A wrapper for an 'EpidemicEvent' to indicate that this is an even that was
-- observed rather than just an event of the epidemic process.
newtype Observation =
  Observation EpidemicEvent
  deriving (Show, Eq, Generic)

instance Json.FromJSON Observation

instance Json.ToJSON Observation

observedEvents :: [EpidemicEvent] -> Either String [Observation]
observedEvents = undefined
