module Physics (
  simulate
) where

import Data.List
import Types

timeScaleFactor = 31536000
granularity = 0.001
gravitationalConstant = 6.67384e-11

simulate :: [Body] -> Double -> [Body]
simulate = turnCrank
  where turnCrank ps t | t <= 0    = ps
                       | otherwise = turnCrank (updateDynamics ps granularity) (t - granularity)

updateDynamics :: [Body] -> Double -> [Body]
updateDynamics particles dt = zipWith4 update particles as vs xs
  where 
    scaledDt = dt * timeScaleFactor
    as = gravitationalField $ map (\p -> (pos p, mass p)) particles
    vs = zipWith (\a v -> v |+| (a |*| scaledDt)) as $ map vel particles
    xs = zipWith (\v x -> x |+| (v |*| scaledDt)) vs $ map pos particles
    update p a v x = Body { col = col p, rad = rad p, mass = mass p, pos = x, vel = v, acc = a }

gravitationalField :: [(Vec,Double)] -> [Vec]
gravitationalField particles = map (\(p,m) -> fieldAt p $ delete (p,m) particles) particles
  where
    fieldAt posj is = (vecSum $ map (fieldContribution posj) is ) |*| (-gravitationalConstant)
    fieldContribution posj (posi, mi) = (unit $ posj |-| posi) |*| mi |/| absSquare (posi |-| posj)