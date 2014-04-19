{-# LANGUAGE TemplateHaskell #-}
module QueensTest where
import Queens
import Test.QuickCheck
import Test.QuickCheck.All

prop_chooseOneByOne_element_length xs = and [length xs == length ys | ys <- chooseOneByOne xs]
prop_chooseOneByOne_oneTrue xs = and [length (filter id ys) == 1 | ys <- chooseOneByOne xs]
prop_chooseOneByOne_element_count xs = length (chooseOneByOne xs) == length (filter id xs)

prop_dropLast_length xs = null xs || length (dropLast xs) == (length xs - 1)

prop_shiftLeft_length xs = null xs || length (shiftLeft xs) == length (xs)
prop_shiftLeft_lastFalse xs = null xs || not (last $ shiftLeft xs)

prop_shiftRight_length xs = null xs || length (shiftRight xs) == length (xs)
prop_shiftRight_firstFalse xs = null xs || not (head $ shiftRight xs)

golden :: [(Int, Int)]
golden = [(1, 1), (2, 0), (5, 10), (8, 92), (9, 352)]

prop_queensCount = and (map checkCount golden)
  where checkCount (n, expected) = expected == (length $ queens n)

runTests = $quickCheckAll
