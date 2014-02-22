module Graphics (
  Drawable (..),
  circle,
  polygon
) where

import Graphics.UI.GLUT
import Physics

class Drawable a where
  draw :: a -> IO()
  
instance Drawable Particle where
  draw Particle { pos = Vec x y, rad = r } = circle 10 (realToFrac r) (Color3 1 0 0) (realToFrac x, realToFrac y)

circle :: GLdouble -> GLdouble -> Color3 GLfloat -> (GLdouble, GLdouble) -> IO ()
circle res r = do
  polygon [let x = r*sin (2*pi*k/res); y = r*cos (2*pi*k/res) in (x, y) | k <- [1..res]]

polygon :: [(GLdouble, GLdouble)] -> Color3 GLfloat -> (GLdouble, GLdouble) -> IO ()
polygon vs col (xo,yo) = do
  preservingMatrix $ do
    translate $ Vector3 (xo/100.0) (yo/100.0) 0
    color col
    renderPrimitive Polygon $ do
      mapM_ (\(x,y) -> vertex $ Vertex3 (x/100.0) (y/100.0) 0) vs