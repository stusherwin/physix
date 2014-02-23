module Types (
  Colour (..),
  Universe (..),
  Body (..),
  Vector (..),
  Vec (..)
) where

data Colour = Red | Blue | Yellow | White
  deriving (Eq, Show)

type Universe = [Body]
data Body = Body { col :: Colour, pos :: Vec, vel :: Vec, acc :: Vec, rad :: Double, mass :: Double } 
  deriving (Eq, Show)

class Vector a where
  (|+|) :: a -> a -> a
  (|-|) :: a -> a -> a
  (|*|) :: a -> Double -> a
  (|/|) :: a -> Double -> a
  unit :: a -> a
  absSquare :: a -> Double
  vecSum :: [a] -> a
  fromVector :: a -> (Double, Double)

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
  fromVector (Vec x y) = (x, y)