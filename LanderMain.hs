module Main where

import System.Random

import Lander

kSeed = 11

showFlightPath :: [ControlPoint] -> String
showFlightPath cs = "--"

main :: IO ()
main = do
  --print $ map flightScore $ selection flightScore (initialPopulation 100) 10
  printResult 0 (geneticSearch (mkStdGen kSeed))
  where printResult n (x:xs) = do
          if (n `mod` 50) == 0
            then putStrLn $ (show n) ++ ": " ++ (show $ poolRating x) ++ "\n" ++
                 (show $ fly newLander (head x))
            else return ()
          if (n `mod` 500) == 0
            then putStrLn $ show (showFlightPath $ head x)
            else return ()
          printResult (n+1) xs
