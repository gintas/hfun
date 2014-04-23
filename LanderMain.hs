module Main where

import System.Random
import Data.Char
import Data.List
import Data.Array.Base
import Data.Array.ST

import Lander

kSeed = 11

showCharArray :: UArray (Int, Int) Char -> [Char]
showCharArray arr = intercalate "\n" $ splitLines $ elems arr
  where splitLines :: [Char] -> [[Char]]
        splitLines [] = []
        splitLines xs = let (line, rest) = splitAt 100 xs in line : splitLines rest

showFlightPath :: [Lander] -> String
showFlightPath cs = showCharArray $ runSTUArray $ do
  arr <- newArray ((1 :: Int, 1 :: Int), (50, 100)) '.'
  mapM_ (\x -> writeArray arr (charCoord x) (symbol x)) cs
  writeArray arr (mappedCoord landerStartPosition) '>'
  writeArray arr (mappedCoord landingPadPosition) '^'
  return arr
    where mappedCoord (Vector2D x y) = (bounded 1 50 $ round (50.0 - y/2), bounded 1 100 $ round x)
          symbol (Lander p pv r rv) = intToDigit $ bounded 1 9 (round . sqrt $ len2 pv)
          charCoord (Lander p pv r rv) = mappedCoord p
          bounded f t v
            | v < f = f
            | v > t = t
            | otherwise = v

main :: IO ()
main = do
  printResult 0 (geneticSearch (mkStdGen kSeed))
  where printResult n (x:xs) = do
          if (n `mod` 500) == 0
            then let plot = showFlightPath (fly newLander (head x))
                     ratingInfo = (show n) ++ ": " ++ (show $ poolRating x) ++ "\n" ++
                                  (show . last $ fly newLander (head x))
                 in putStrLn $ "_______________\n" ++ plot ++ "\n" ++ ratingInfo
            else return ()
          printResult (n+1) xs
