{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as L
import Data.Csv
import Epidemic ()
import qualified Epidemic.BDSCOD as BDSCOD
import qualified Epidemic.Utility as EpiUtil

main :: IO ()
main =
  let simConfig =
        BDSCOD.configuration
          4.1
          (2.5, 0.2, 0.15, [(3, 0.5), (4, 0.5)], 0.2, [(3.5, 0.5)])
   in do events <- EpiUtil.simulationWithSystemRandom simConfig BDSCOD.allEvents
         L.writeFile "demo-output-all-events.csv" (encode events)
         L.writeFile "demo-output-observed-events.csv" . encode $
           BDSCOD.observedEvents events
         return ()
