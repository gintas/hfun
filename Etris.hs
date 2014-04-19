{-# LANGUAGE TemplateHaskell #-}

module Etris where
import Data.List
import Test.QuickCheck
import Test.QuickCheck.All

data Cell = Empty | Solid | Moving deriving (Eq)

newtype Board = Board [[Cell]] deriving (Eq)

instance Show Cell where
  show Empty = "."
  show Solid = "X"
  show Moving = "x"

instance Show Board where
  show (Board xss) = intercalate "\n" $ map showLine xss
    where showLine xs = concat (map show xs)

data Coord = Coord Int Int
topLeft = Coord 0 0

figL :: Board
figL = Board [[Moving, Moving, Moving],
              [Empty, Empty, Moving]]

figS :: Board
figS = Board [[Moving, Moving, Empty],
              [Empty, Moving, Moving]]

--figL = read $ "xxx\n..x"
--figS :: Board
--figS = read $ "xx.\n.xx"

mkBoard :: Int -> Int -> Board
mkBoard h w = Board $ repeatN h (repeatN w Empty)

height (Board ls) = length ls
width (Board []) = 0
width (Board ls) = length (head ls)

rotate :: Board -> Board
rotate (Board b) = Board (rotate_ b)
  where rotate_ ([]:_) = []
        rotate_ ls = (reverse $ map head ls) : rotate_ (map tail ls)

rotate2 = rotate . rotate

shift :: Int -> Int -> Board -> Board
shift y x (Board cs) =
  Board $ addLeftPadding $ (repeatN y emptyRow ++ cs)
  where emptyRow :: [Cell]
        emptyRow = repeatN (width (Board cs)) Empty
        addLeftPadding :: [[Cell]] -> [[Cell]]
        addLeftPadding rows = map (prefix ++) rows
        prefix :: [Cell]
        prefix = repeatN x Empty

moveDown = shift 1 0
moveRight n = shift 0 n

place :: Board -> Board -> Board
place (Board fs) (Board cs) =
  Board $ map overlapLine (zipDefault [] fs cs)
  where overlapLine (as, bs) = map overlapCell (zipDefault Empty as bs)
        overlapCell (Empty, b) = b
        overlapCell (a, b) = a

overlaps :: Board -> Board -> Bool
overlaps (Board as) (Board bs) =
  any overlapsLine $ zipDefault [] as bs
  where overlapsLine (as_, bs_) = any overlapsCell (zipDefault Empty as_ bs_)
        overlapsCell (Empty, b) = False
        overlapsCell (a, Empty) = False
        overlapsCell (a, b) = True

endMove :: Board -> Board
endMove (Board bs) = Board (map (map endMoveInCell) bs)
                     where endMoveInCell Empty = Empty
                           endMoveInCell Solid = Solid
                           endMoveInCell Moving = Solid

dropPiece :: Board -> Board -> Board
dropPiece f b =
  let lowered_f = moveDown f in
  if overlaps b lowered_f || height lowered_f > height b then
    endMove (place b f)
  else
    dropPiece lowered_f b

strikeout :: Board -> Board
strikeout (Board bs) =
  shift n 0 (strikeout_ bs)
  where strikeout_ bs = Board (filter (any (/= Solid)) bs)
        n = length $ filter (all (== Solid)) bs

defaultBoard = mkBoard 8 8

data Game = Game Board [Board -> Board]
game :: Game
game = Game defaultBoard [ (dropPiece . rotate2) figL
                         , (dropPiece . moveRight 2) figS
                         , (dropPiece . moveRight 5 . rotate2) figL
                         ]

play :: Game -> Board
play (Game b []) = b
play (Game b (f:fs)) = play (Game (strikeout (f b)) fs)

main = putStrLn $ show $ play game

-- Utilities
repeatN n v = take n $ repeat v

headDefault :: a -> [a] -> a
headDefault d [] = d
headDefault d (a:as) = a

tailDefault :: [a] -> [a]
tailDefault [] = []
tailDefault as = tail as

zipDefault :: a -> [a] -> [a] -> [(a, a)]
zipDefault d [] [] = []
zipDefault d as bs =
  ((headDefault d as), (headDefault d bs)) : (zipDefault d (tailDefault as) (tailDefault bs))

-- Tests
-- prop_boardShow h w =
--   h > 0 && h < 10 && w > 0 && w < 10 ==>
--   show (mkBoard h w) == (intercalate "\n" $ repeatN h (repeatN w '.'))
-- prop_boardHeight h w =
--   h > 0 && h < 10 && w > 0 && w < 10 ==>
--   h == (height (mkBoard h w))
-- prop_boardWidth h w =
--   h > 0 && h < 10 && w > 0 && w < 10 ==>
--   w == (width (mkBoard h w))

prop_overlapEmpty =
  overlaps (mkBoard 5 5) figL == False

prop_overlapSelf =
  overlaps figL figL == True

prop_overlapSelf1 =
  overlaps figL (shift 1 0 figL) == True

prop_overlapSelf2 =
  overlaps figL (shift 2 0 figL) == False

prop_rotate4 =
  (rotate . rotate . rotate . rotate) figL == figL

--prop_zipDefault a b = 

runTests = $quickCheckAll
