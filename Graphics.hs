module Graphics (
  Drawable (..),
  circle,
  polygon
) where

import Graphics.UI.GLUT
import Types as T

minRes = 10
maxRes = 100
minRadius = 0.005

class Drawable a where
  draw :: (Double,Double) -> Double -> a -> IO()
  
instance Drawable Body where
  draw (origx,origy) scaleFactor Body { pos = Vec x y, rad = r, col = c } = 
    let radius     = r * scaleFactor
        resolution = max minRes $ min maxRes $ (maxRes - minRes) * radius + minRes
        position   = ((origx + x) * scaleFactor, (origy + y) * scaleFactor)
    in if radius > minRadius then circle resolution radius c position
                             else point c position

circle :: Double -> Double -> T.Colour -> (Double, Double) -> IO ()
circle res r col (x,y) = do
  let vertices = [let x = r*sin (2*pi*k/res); y = r*cos (2*pi*k/res) in (realToFrac x, realToFrac y) | k <- [1..res]]
  let colour = (fromColour col)
  let position = (realToFrac x, realToFrac y)
  polygon vertices colour position

point :: T.Colour -> (Double, Double) -> IO ()
point col (x,y) = do
  preservingMatrix $ do
    translate $ Vector3 (realToFrac x) (realToFrac y) (0 :: GLdouble)
    color $ fromColour col
    renderPrimitive Points $ do
      vertex $ Vertex3 0 0 (0 :: GLdouble)

fromColour :: T.Colour -> Color3 GLfloat
fromColour T.Red    = Color3 1 0 0
fromColour T.Yellow = Color3 1 1 0
fromColour T.Blue   = Color3 0 0 1
fromColour T.White  = Color3 1 1 1

polygon :: [(GLdouble, GLdouble)] -> Color3 GLfloat -> (GLdouble, GLdouble) -> IO ()
polygon vs col (xo,yo) = do
  preservingMatrix $ do
    translate $ Vector3 xo yo 0
    color col
    renderPrimitive Polygon $ do
      mapM_ (\(x,y) -> vertex $ Vertex3 x y 0) vs