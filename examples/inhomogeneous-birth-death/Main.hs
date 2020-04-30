{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as L
import Data.Csv
import Data.Maybe
import Epidemic ()
import qualified Epidemic.InhomogeneousBD as InhomBD
import qualified Epidemic.Utility as EpiUtil

main :: IO ()
main =
  let simDur = 7.0
      numBreaks = 10
      simTimes = [simDur * (i / numBreaks) | i <- [0..(numBreaks-1)]]
      simBRs = [1 + cos (pi * i / numBreaks) | i <- [0..(numBreaks-1)]]
      simBRTs = zip simTimes simBRs
      simDR = 0.4
      simConfig = fromJust $ InhomBD.configuration simDur (simBRTs, simDR)
   in do events <- EpiUtil.simulation simConfig InhomBD.allEvents
         L.writeFile "demo-output-all-events.csv" (encode events)
         return ()
