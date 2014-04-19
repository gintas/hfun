module Main where

import Queens

main :: IO ()
main = do
  putStrLn $ show $ length (queens 12)
