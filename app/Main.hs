{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Csv
import qualified Data.ByteString.Lazy as B
import Epidemic
import Epidemic.Simulation
import Epidemic.BirthDeath


main :: IO ()
main = do
  events <- birthDeathSimulation $ birthDeathConfig 5 1 0.3
  B.writeFile "demo-output.csv" (encode events)
  return ()
