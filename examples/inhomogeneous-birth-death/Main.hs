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
      simTimes = [0, simDur / 2]
      simDR = 1.0
      simBRs = [3.0, simDR - 0.5]
      simBRTs = zip simTimes simBRs
      simConfig = fromJust $ InhomBD.configuration simDur (simBRTs, simDR)
   in do events <- EpiUtil.simulation simConfig InhomBD.allEvents
         L.writeFile "demo-output-all-events.csv" (encode events)
         return ()
