module Main where

import Graphics.UI.GLUT
import Control.Monad
import Data.IORef
import Data.Time.Clock
import Data.List
import Data.Char
import Types as T
import Physics
import Graphics

deltaTimeResolution = 0.01
distanceScaleExponent = -12
origin = (0, 0)
paused = False
track = Just 0

sun     = Body { col = T.Yellow, rad = 6.96e8, mass = 1.98e30, pos = Vec 0 0,                    vel = Vec 0 0,                    acc = Vec 0 0 }
jupiter = Body { col = T.Red,    rad = 6.99e7, mass = 1.89e27, pos = Vec (-7.78e11) 0,           vel = Vec 0 (-1.3e4),             acc = Vec 0 0 }
earth   = Body { col = T.Blue,   rad = 6.37e6, mass = 5.97e24, pos = Vec 1.49e11 0,              vel = Vec 0 2.97e4,               acc = Vec 0 0 }
moon    = Body { col = T.White,  rad = 1.73e6, mass = 7.34e22, pos = pos earth |+| Vec 3.84e8 0, vel = vel earth |+| Vec 0 1.02e3, acc = Vec 0 0 }

universe :: Universe
universe = [sun, jupiter, earth, moon]

main :: IO ()
main = do
  (_progname, _) <- getArgsAndInitialize
  _window <- createWindow "Physix"
  currentTime <- getCurrentTime
  lastTime' <- newIORef $ utctDayTime currentTime
  universe' <- newIORef universe
  distanceScaleExponent' <- newIORef distanceScaleExponent
  origin' <- newIORef origin
  paused' <- newIORef paused
  track' <- newIORef track
  displayCallback $= display origin' distanceScaleExponent' universe' track'
  reshapeCallback $= Just reshape
  idleCallback $= Just (idle lastTime' universe' paused')
  keyboardMouseCallback $= Just (keyboardMouse origin' distanceScaleExponent' paused' track')
  mainLoop

display :: IORef (Double, Double) -> IORef Double -> IORef Universe -> IORef (Maybe Int) -> DisplayCallback
display origin' distanceScaleExponent' universe' track' = do
  clear [ ColorBuffer ]
  universe <- get universe'
  origin <- get origin'
  track <- get track'
  distanceScaleExponent <- get distanceScaleExponent'
  let scaleFactor = 10.0 ** distanceScaleExponent
  mapM_ (draw (findOrigin origin track universe) scaleFactor) $ universe
  flush
  where
    findOrigin origin Nothing _ = origin
    findOrigin origin (Just t) universe 
      | t >= 0 && t < length universe = fromVector ( (Vec 0 0) |-| (pos (universe !! t)))
      | otherwise                     = origin

reshape :: ReshapeCallback
reshape (Size w h) = do
  viewport $= (Position 0 0, Size w h)
  let width = fromIntegral w
  let height = if h == 0 then 1.0 else fromIntegral h
  let aspect = width / height
  matrixMode $= Projection
  loadIdentity
  if width >= height
    then ortho2D ((-1.0) * aspect) (1.0 * aspect) (-1.0) 1.0
    else ortho2D (-1.0) 1.0 ((-1.0) / aspect) (1.0 / aspect)
  postRedisplay Nothing

idle :: IORef DiffTime -> IORef Universe -> IORef Bool -> IdleCallback
idle lastTime' universe' paused' = do
  universe <- get universe'
  lastTime <- get lastTime'
  time <- (getCurrentTime >>= (return . utctDayTime))
  paused <- get paused'
  let deltaTime = time - lastTime
  if deltaTime > deltaTimeResolution
    then do
      writeIORef lastTime' time 
      if paused 
        then return ()
        else writeIORef universe' $ simulate universe $ seconds deltaTime
    else return ()
  postRedisplay Nothing
  where
    seconds t = fromRational $ toRational t

keyboardMouse :: IORef (Double, Double) -> IORef Double -> IORef Bool -> IORef (Maybe Int) -> KeyboardMouseCallback
keyboardMouse origin' distanceScaleExponent' paused' track' key Down _ _ = do
  distanceScaleExponent <- get distanceScaleExponent'
  let scaleFactor = 10.0 ** (-distanceScaleExponent)
  case key of
    (Char '=') -> distanceScaleExponent' $~! (+ 0.1)
    (Char '-') -> distanceScaleExponent' $~! (subtract 0.1)
    (SpecialKey KeyUp)    -> origin' $~! (\(x,y) -> (x,y + 0.1 * scaleFactor))
    (SpecialKey KeyDown)  -> origin' $~! (\(x,y) -> (x,y - 0.1 * scaleFactor))
    (SpecialKey KeyLeft)  -> origin' $~! (\(x,y) -> (x - 0.1 * scaleFactor,y))
    (SpecialKey KeyRight) -> origin' $~! (\(x,y) -> (x + 0.1 * scaleFactor,y))
    (Char ' ') -> paused' $~! not
    (Char num) | num >= '0' && num <= '9' -> writeIORef track' $ Just $ ord num - ord '0'
    _ -> return ()
keyboardMouse _ _ _ _ _ _ _ _ = return ()