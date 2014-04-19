module Queens where

import Data.List
import Control.Monad.Par (parMap, runPar, NFData)

data Queen = Queen | NoQueen

data RowInfo = RowInfo { rQueen :: [Bool], rDiagLeft :: [Bool], rVertical :: [Bool], rDiagRight :: [Bool] }
             deriving (Show)

type Solution = [[Bool]]

(|-) :: a -> (a -> b) -> b
x |- f = f x

emptyRow :: Int -> RowInfo
emptyRow cols = RowInfo noData noData noData noData
  where noData = take cols (repeat False)

type MapStrat = Int -> (RowInfo -> [Solution]) -> [RowInfo] -> [[Solution]]
simpleStrat d = map
naiveParStrat d f xs = runPar $ parMap f xs
cutoffParStrat d = if d > 6 then naiveParStrat d else simpleStrat d
strategies :: [MapStrat]
strategies = [simpleStrat, naiveParStrat, cutoffParStrat]

queens d = map tail $ solveQueens cutoffParStrat d (emptyRow d)
--queens d = map tail $ solveQueens naiveParStrat d (emptyRow d)

solveQueens :: MapStrat -> Int -> RowInfo -> [Solution]
solveQueens _ 0 prevRowInfo = [[rQueen prevRowInfo]]
solveQueens mapStrategy rows prevRowInfo =
  let newRows = validMoves prevRowInfo
      solver = solveQueens mapStrategy (rows - 1)
      validSuffixes = concat $ map' solver newRows
  in map (rQueen prevRowInfo :) validSuffixes
  where map' f xs = mapStrategy rows f xs

validMoves :: RowInfo -> [RowInfo]
validMoves prevRowInfo =
  let diagLeft = shiftLeft (rDiagLeft prevRowInfo)
      diagRight = shiftRight (rDiagRight prevRowInfo)
      vertical = rVertical (prevRowInfo)
      available = map not $ map or3 $ zip3 diagLeft diagRight vertical
      moves = chooseOneByOne available
  in [let m xs = map or2 (zip move xs) in
       RowInfo move (m diagLeft) (m vertical) (m diagRight) | move <- moves]
     where or2 (a, b) = a || b
           or3 (x, y, z) = x || y || z

dropLast [] = undefined
dropLast [x] = []
dropLast (x:xs) = x : dropLast xs
-- TODO: rewrite using foldr

shiftLeft row = tail row ++ [False]

shiftRight row = False : (dropLast row)

chooseOneByOne :: [Bool] -> [[Bool]]
chooseOneByOne xs =
  let pos = positions xs 0 []
  in [(take n falses) ++ [True] ++ (take (len-n-1) falses) | n <- pos]
  where falses = repeat False
        len = length xs
        positions [] i result = result
        positions (y:ys) i result = positions ys (i+1) newResult
          where newResult = if y then (i:result) else result


showSolution :: Solution -> String
showSolution rows = intercalate "\n" (map rowstr rows)
  where rowstr = map repr
        repr x = if x then 'X' else '.'

p d = putStrLn $ intercalate "\n\n" $ map showSolution (queens d)
cs d = length (queens d)
