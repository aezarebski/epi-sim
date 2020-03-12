{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Csv
import Data.List (sort)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Epidemic
import Epidemic.Utility
import Epidemic.BirthDeathSamplingOccurrence


main :: IO ()
main = do
  events <- birthDeathSamplingOccurrenceSimulation $ birthDeathSamplingOccurrenceConfig 6 (1.3,0.1,0.1,0.2)
  L.writeFile "demo-output.csv" (encode events)
  B.writeFile "demo-output.json" (eventsAsJsonTree events)
  L.writeFile "demo-output-sample-tree.csv" $ encode . sampleTreeEvents . sampleTree $ transmissionTree events (Person 1)
  return ()
