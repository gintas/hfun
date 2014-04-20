module Lander where

import System.Random
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

data ControlPoint = ControlPoint Double Double deriving (Eq, Show)

-- Constants
boundaries = Vector2D 100.0 100.0
landerStartPosition = Vector2D 10.0 80.0
landingPadPosition = Vector2D 50.0 0.0
gAcceleration = Vector2D 0.0 (-9.8)
defaultStep = 0.2 :: Duration -- s

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
    fly' ((ControlPoint pa ra):cs) t dt lander = trace (show $ lander) $
     if inBounds boundaries $ landerPos lander
     then let pav = gAcceleration + angleVector (landerAngle lander) pa in
       fly' cs (t+dt) dt $ updateVelocities defaultStep pav ra $ moveLander defaultStep lander
     else (lander, t)
-- TODO: refactor using a fold

calculateScore :: Duration -> Lander -> Score
calculateScore t (Lander (Vector2D x y) pv r rv) =
  if not $ inBounds boundaries p' then 0
  else len2 (landingPadPosition - p') + len2 pv + r*r  -- TODO: weights
  where p' = Vector2D x 0

nullControls = (ControlPoint 0.0 0.0) : nullControls

multiplex2 (x1:x2:xs) = (x1, x2):(multiplex2 xs)

randomControls :: StdGen -> [ControlPoint]
randomControls g =
  let rs = randomRs (-1 :: Int, 1) g
  in map cp (multiplex2 rs)
  where cp (r1, r2) = ControlPoint pa ra
                      where pa = case r1 of
                              -1 -> 0
                              0 -> 5
                              1 -> 15
                            ra = case r2 of
                              -1 -> -45
                              0 -> 0
                              1 -> 45

main :: IO ()
main =
  print $ calculateScore t lander
  where (lander, t) = fly newLander (randomControls $ mkStdGen 1)
