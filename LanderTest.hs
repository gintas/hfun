{-# LANGUAGE TemplateHaskell #-}
module Main where
import Lander
import Test.QuickCheck
import Test.QuickCheck.All

prop_smoketest = poolRating (geneticSearch !! 5) > poolRating (geneticSearch !! 10)

runTests = $quickCheckAll

main = runTests
