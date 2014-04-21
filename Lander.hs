module Main where

import Control.Monad.Par (parMap, runPar, NFData)
import Data.Function
import Data.List
import System.Random
import System.IO.Unsafe
import Debug.Trace -- XXX

type Angle = Double
type Duration = Double

data Vector2D = Vector2D Double Double deriving (Eq, Show)

instance Num Vector2D where
  (Vector2D x1 y1) + (Vector2D x2 y2) = Vector2D (x1+x2) (y1+y2)
  (Vector2D x1 y1) - (Vector2D x2 y2) = Vector2D (x1-x2) (y1-y2)
  (Vector2D x1 y1) * (Vector2D x2 y2) = undefined
  abs (Vector2D x y) = Vector2D (abs x) (abs y)
  signum (Vector2D _ _) = undefined
  fromInteger 0 = Vector2D 0.0 0.0
  fromInteger _ = undefined

angleVector :: Angle -> Double -> Vector2D
angleVector a d = Vector2D (d * cos a) (d * sin a)

scale :: Vector2D -> Double -> Vector2D
scale (Vector2D x y) k = Vector2D (x*k) (y*k)

len2 :: Vector2D -> Double
len2 (Vector2D x y) = x*x + y*y

dist v1 v2 = len2 (v1 - v2)

data Lander = Lander { landerPos :: Vector2D
                     , landerPosV :: Vector2D
                     , landerAngle :: Angle
                     , landerAngleV :: Angle
                     } deriving (Eq, Show)

data ControlPoint = ControlPoint Double Double deriving (Eq, Ord, Show)

-- Constants
boundaries = Vector2D 100.0 100.0
landerStartPosition = Vector2D 10.0 80.0
landingPadPosition = Vector2D 50.0 0.0
gAcceleration = Vector2D 0.0 (-10)
defaultStep = 0.2 :: Duration -- s

-- Parameters for the genetic algorithm
kPopulation = 200
kSelected = 30
kMutation = 0.1

inBounds :: Vector2D -> Vector2D -> Bool
inBounds boundaries v = (v == abs v) && (v' == abs v')
  where v' = boundaries - v

newLander = Lander landerStartPosition 0 0 0

moveLander :: Duration -> Lander -> Lander
moveLander dt l = Lander
                  (landerPos l + scale (landerPosV l) dt)
                  (landerPosV l)
                  (landerAngle l + (landerAngleV l) * dt)
                  (landerAngleV l)

updateVelocities :: Duration -> Vector2D -> Angle -> Lander -> Lander
updateVelocities dt pa ra (Lander p pv r rv) = Lander p (pv + (scale pa dt)) r (rv + ra * dt)

type Score = Double

fly :: Lander -> [ControlPoint] -> (Lander, Duration)
fly lander cs = fly' cs 0.0 defaultStep lander
  where
    fly' ((ControlPoint pa ra):cs) t dt lander =
     --trace (show $ lander) $
     if inBounds boundaries $ landerPos lander
     then let pav = gAcceleration + angleVector (landerAngle lander) pa in
       fly' cs (t+dt) dt $ updateVelocities defaultStep pav ra $ moveLander defaultStep lander
     else (lander, t)
-- TODO: refactor using a fold

calculateScore :: Duration -> Lander -> Score
calculateScore t (Lander (Vector2D x y) pv r rv) =
  if not $ inBounds boundaries p' then inf
  else len2 (landingPadPosition - p') + 5 * len2 pv + r*r  -- TODO: weights
  where p' = Vector2D x 0
        inf = 999999999

nullControls = (ControlPoint 0.0 0.0) : nullControls

multiplex2 (x1:x2:xs) = (x1, x2):(multiplex2 xs)

createControlPoint :: (Int, Int) -> ControlPoint
createControlPoint (r1, r2) = ControlPoint pa ra
  where pa = case r1 of
          -1 -> 0
          0 -> 10
          1 -> 20
        ra = case r2 of
          -1 -> -45
          0 -> 0
          1 -> 45

randomControls :: StdGen -> [ControlPoint]
randomControls g =
  let rs = randomRs (-1 :: Int, 1) g
  in map createControlPoint (multiplex2 rs)

type Probability = Double

flightScore :: [ControlPoint] -> Score
flightScore cs = calculateScore t lander
  where (lander, t) = fly newLander cs

selection :: (a -> Score) -> [a] -> Int -> [a]
selection scorer css m = map snd $ take m $ sortBy (compare `on` fst) $
                         zip (runPar $ parMap scorer css) css

combine :: Probability -> StdGen -> [a] -> [a] -> [a]
combine p g (c1:cs1) (c2:cs2) =
  c : (combine p g' cs1 cs2)
  where (r, g') = random g
        c = if r > p then c1 else c2

randomPair :: StdGen -> Int -> (Int, Int, StdGen)
randomPair g n =
  if i1 /= i2 then (i1, i2, g'') else randomPair g'' n
  where (i1, g') = randomR (0, n) g
        (i2, g'') = randomR (0, n) g'

recombination :: StdGen -> [[ControlPoint]] -> Int -> [[ControlPoint]]
recombination _ _ 0 = []
recombination g ccs n =
  let entry = combine 0.5 g' (ccs !! i1) (ccs !! i2)
      rest = recombination g' ccs (n - 1)
  -- in trace (show i1 ++ " " ++ show i2) $
  in entry : rest
  where (i1, i2, g') = randomPair g (length ccs - 1)
-- TODO: might be better to combine by splicing subsequences

mutation :: StdGen -> [[ControlPoint]] -> [[ControlPoint]]
mutation g ccs = map mutate ccs
  where mutate cs = combine kMutation g cs rnd
        rnd = randomControls g
-- TODO: avoid conflating translation and rotation actions into one

runBatch :: [[ControlPoint]] -> [[ControlPoint]]
runBatch ccs =
  let top = selection flightScore ccs kSelected
      mutated = mutation (unsafePerformIO newStdGen) top
      new = recombination (unsafePerformIO newStdGen) top (kPopulation - kSelected - kSelected)
  in top ++ mutated ++ new

initialPopulation :: Int -> [[ControlPoint]]
initialPopulation n = map (\s -> randomControls $ (unsafePerformIO newStdGen)) [1..n]

geneticSearch :: [[[ControlPoint]]]
geneticSearch = iterate runBatch (initialPopulation kPopulation)

poolRating :: [[ControlPoint]] -> Score
poolRating ccs = sum (map flightScore $ take kSelected ccs) / (fromIntegral kSelected)

main :: IO ()
main =
  --print $ map flightScore $ selection flightScore (initialPopulation 100) 10
  printResult 0 geneticSearch
  where printResult n (x:xs) = do
          if (n `mod` 50) == 0
            then putStrLn $ (show n) ++ ": " ++ (show $ poolRating x) ++ "\n" ++ (show $ fly newLander (head x))
            else return ()
          printResult (n+1) xs
