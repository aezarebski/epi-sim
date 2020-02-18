{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Csv
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Epidemic.Utility
import Epidemic.BirthDeath


main :: IO ()
main = do
  events <- birthDeathSimulation $ birthDeathConfig 6 1 0.3
  L.writeFile "demo-output.csv" (encode events)
  B.writeFile "demo-output.json" (eventsAsJsonTree events)
  return ()
