module Physics (
  simulate
) where

import Data.List
import Types

timeScaleFactor = 3153600.0 -- 1 sec = 1 year
granularity = 0.001
gravitationalConstant = 6.67384e-11

simulate :: [Body] -> Double -> [Body]
simulate = turnCrank
  where turnCrank ps t | t <= 0    = ps
                       | otherwise = turnCrank (updateDynamics ps granularity) (t - granularity)

updateDynamics :: [Body] -> Double -> [Body]
updateDynamics bodies dt = zipWith4 (\p a v x -> p { pos = x, vel = v, acc = a}) bodies as vs xs
  where 
    scaledDt = dt * timeScaleFactor
    as = gravitationalField $ map (\p -> (pos p, mass p)) bodies
    vs = zipWith (\a v -> v |+| (a |*| scaledDt)) as $ map vel bodies
    xs = zipWith (\v x -> x |+| (v |*| scaledDt)) vs $ map pos bodies

gravitationalField :: [(Vec,Double)] -> [Vec]
gravitationalField bodies = map (\(p,m) -> fieldAt p $ delete (p,m) bodies) bodies
  where
    fieldAt posj is = (vecSum $ map (fieldContribution posj) is ) |*| (-gravitationalConstant)
    fieldContribution posj (posi, mi) = (unit $ posj |-| posi) |*| mi |/| absSquare (posi |-| posj)