{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Csv
import qualified Data.ByteString.Lazy as L
import Epidemic
import Epidemic.Utility
import Epidemic.BDSCOD


main :: IO ()
main = do
  events <- simulation $ configuration 4.5 (2.1,0.2,0.1,[(3,0.5), (4,0.5)],0.2,[(3.5,0.5)])
  L.writeFile "demo-output-all-events.csv" (encode events)
  L.writeFile "demo-output-observed-events.csv" . encode $ observedEvents events
  return ()
