{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Epidemic.Types.Parameter where

import qualified Data.Aeson as Json
import qualified Data.Csv as Csv
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Epidemic.Types.Population (Population(..))
import Epidemic.Types.Time (AbsoluteTime(..), TimeDelta(..), timeDelta)
import GHC.Generics

class (Population p) =>
      ModelParameters a p
  where
  rNaught :: p -> a -> AbsoluteTime -> Maybe Double
  eventRate :: p -> a -> AbsoluteTime -> Maybe Rate
  birthProb :: p -> a -> AbsoluteTime -> Maybe Probability

type Rate = Double

type Probability = Double
