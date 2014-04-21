module Main where

import Lander

main :: IO ()
main =
  --print $ map flightScore $ selection flightScore (initialPopulation 100) 10
  printResult 0 geneticSearch
  where printResult n (x:xs) = do
          if (n `mod` 50) == 0
            then putStrLn $ (show n) ++ ": " ++ (show $ poolRating x) ++ "\n" ++ (show $ fly newLander (head x))
            else return ()
          printResult (n+1) xs
