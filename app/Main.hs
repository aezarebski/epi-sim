{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Csv
import qualified Data.ByteString.Lazy as L
import Epidemic
import Epidemic.Utility
import Epidemic.BirthDeathSamplingCatastropheOccurrence


main :: IO ()
main = do
  events <- bdscoSimulation $ bdscoConfig 4 (1.3,0.1,0.1,[(3,0.5)],0.2)
  L.writeFile "demo-output.csv" (encode events)
  L.writeFile "demo-output-sample-tree.csv" . encode $ bdscoObservedEvents events
  return ()
