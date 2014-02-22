module Physics (
  World,
  Particle (..),
  Vector (..),
  Vec (..),
  simulate
) where

import Data.List

granularity = 0.001
gravitationalConstant = 6.67384e-11

type World = [Particle]
data Particle = Particle { pos :: Vec, vel :: Vec, acc :: Vec, rad :: Double, mass :: Double } 
  deriving (Eq, Show)

class Vector a where
  (|+|) :: a -> a -> a
  (|-|) :: a -> a -> a
  (|*|) :: a -> Double -> a
  (|/|) :: a -> Double -> a
  unit :: a -> a
  absSquare :: a -> Double
  vecSum :: [a] -> a

data Vec = Vec Double Double deriving (Eq, Show)

instance Vector Vec where
  (Vec x1 y1) |+| (Vec x2 y2) = Vec (x1 + x2) (y1 + y2)
  (Vec x1 y1) |-| (Vec x2 y2) = Vec (x1 - x2) (y1 - y2)
  (Vec x y) |*| n = Vec (x * n) (y * n)
  (Vec x y) |/| n = Vec (x / n) (y / n)
  unit (Vec x y) = 
    let norm = sqrt (absSquare (Vec x y))
    in Vec (x/norm) (y/norm)
  absSquare (Vec x y) = x^2 + y^2
  vecSum = foldl (|+|) (Vec 0 0)

simulate :: [Particle] -> Double -> [Particle]
simulate = turnCrank
  where turnCrank ps t | t <= 0    = ps
                       | otherwise = turnCrank (updateDynamics ps granularity) (t - granularity)

updateDynamics :: [Particle] -> Double -> [Particle]
updateDynamics particles dt = zipWith4 update particles as vs xs
  where 
    as = gravitationalField $ map (\p -> (pos p, mass p)) particles
    vs = zipWith (\a v -> v |+| (a |*| dt)) as $ map vel particles
    xs = zipWith (\v x -> x |+| (v |*| dt)) vs $ map pos particles
    update p a v x = Particle { rad = rad p, mass = mass p, pos = x, vel = v, acc = a }

gravitationalField :: [(Vec,Double)] -> [Vec]
gravitationalField particles = map (\(p,m) -> fieldAt p $ delete (p,m) particles) particles
  where
    fieldAt posj is = (vecSum $ map (fieldContribution posj) is ) |*| (-gravitationalConstant)
    fieldContribution posj (posi, mi) = (unit $ posj |-| posi) |*| mi |/| absSquare (posi |-| posj)