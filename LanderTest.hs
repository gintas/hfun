{-# LANGUAGE TemplateHaskell #-}
module Main where
import Lander
import System.Random
import Test.QuickCheck
import Test.QuickCheck.All

g = mkStdGen 11
result = geneticSearch g

bestFlightScore ccs = flightScore $ head ccs

prop_smoketest = bestFlightScore (result !! 5) > bestFlightScore (result !! 25)

runTests = $quickCheckAll

main = runTests
