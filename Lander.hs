module Lander where

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

fly :: Lander -> (Lander, Duration)
fly lander = fly' 0.0 defaultStep lander
  where
    fly' t dt lander = trace (show $ landerPos lander) $
     if inBounds boundaries $ landerPos lander
     then fly' (t+dt) dt $
       updateVelocities defaultStep gAcceleration 0.0 $
       moveLander defaultStep lander
     else (lander, t)

-- TODO: take time into account
calculateScore :: Duration -> Vector2D -> Score
calculateScore t (Vector2D x y) = len2 (landingPadPosition - pos')
  where pos' = Vector2D x 0

output = putStrLn . show

main :: IO ()
main = output $ calculateScore t (landerPos lander)
  where (lander, t) = fly newLander
  --putStrLn "Hello Lander"
