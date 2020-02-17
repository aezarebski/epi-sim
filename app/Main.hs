module Main where

import Epidemic
import Epidemic.Simulation
import Epidemic.BirthDeath


main :: IO ()
main = do
  events <- birthDeathSimulation $ birthDeathConfig 5 1 0.3
  print events
  return ()
