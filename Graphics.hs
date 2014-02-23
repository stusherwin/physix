module Graphics (
  Drawable (..),
  circle,
  polygon
) where

import Graphics.UI.GLUT
import Types as T

class Drawable a where
  draw :: (Double,Double) -> Double -> a -> IO()
  
instance Drawable Body where
  draw (origx,origy) scaleFactor Body { pos = Vec x y, rad = r, col = c } = 
    let radius   = realToFrac (max 0.005 (r * scaleFactor))
        position = (realToFrac (origx + x * scaleFactor), realToFrac (origy + y * scaleFactor))
        colour T.Red    = Color3 1 0 0
        colour T.Yellow = Color3 1 1 0
        colour T.Blue   = Color3 0 0 1
        colour T.White  = Color3 1 1 1
    in circle 10 radius (colour c) position

circle :: GLdouble -> GLdouble -> Color3 GLfloat -> (GLdouble, GLdouble) -> IO ()
circle res r = do
  polygon [let x = r*sin (2*pi*k/res); y = r*cos (2*pi*k/res) in (x, y) | k <- [1..res]]

polygon :: [(GLdouble, GLdouble)] -> Color3 GLfloat -> (GLdouble, GLdouble) -> IO ()
polygon vs col (xo,yo) = do
  preservingMatrix $ do
    translate $ Vector3 xo yo 0
    color col
    renderPrimitive Polygon $ do
      mapM_ (\(x,y) -> vertex $ Vertex3 x y 0) vs