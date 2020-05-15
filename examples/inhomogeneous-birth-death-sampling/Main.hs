{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as L
import Data.Csv
import Data.Maybe
import Epidemic ()
import qualified Epidemic.InhomogeneousBDS as InhomBDS
import qualified Epidemic.Utility as EpiUtil

main :: IO ()
main =
  let simDur = 7.0
      simTimes = [0, simDur / 2]
      simSR = 1.0
      simDR = 1.0
      simBRs = [4.0, (simDR + simSR) - 0.5]
      simBRTs = zip simTimes simBRs
      simConfig = fromJust $ InhomBDS.configuration simDur (simBRTs, simDR, simSR)
   in do events <- EpiUtil.simulationWithSystemRandom False simConfig InhomBDS.allEvents
         L.writeFile "demo-output-all-events.csv" (encode events)
         return ()
