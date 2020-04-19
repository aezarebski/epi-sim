{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Csv
import qualified Data.ByteString.Lazy as L
import Epidemic
import Epidemic.Utility
import qualified Epidemic.BDSCOD as BDSCOD


main :: IO ()
main = do
  events <- simulation (BDSCOD.configuration 4.5 (2.1,0.2,0.1,[(3,0.5), (4,0.5)],0.2,[(3.5,0.5)])) BDSCOD.allEvents
  L.writeFile "demo-output-all-events.csv" (encode events)
  L.writeFile "demo-output-observed-events.csv" . encode $ BDSCOD.observedEvents events
  return ()
