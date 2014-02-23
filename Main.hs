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

sun     = Body { col = T.Yellow, rad = 6.96e8, mass = 1.98e30, pos = Vec 0 0,                    vel = Vec 0 0,                    acc = Vec 0 0 }
jupiter = Body { col = T.Red,    rad = 6.99e7, mass = 1.89e27, pos = Vec (-7.78e11) 0,           vel = Vec 0 (-1.3e4),             acc = Vec 0 0 }
earth   = Body { col = T.Blue,   rad = 6.37e6, mass = 5.97e24, pos = Vec 1.49e11 0,              vel = Vec 0 2.97e4,               acc = Vec 0 0 }
moon    = Body { col = T.White,  rad = 1.73e6, mass = 7.34e22, pos = pos earth |+| Vec 3.84e8 0, vel = vel earth |+| Vec 0 1.02e3, acc = Vec 0 0 }

data State = State { planets :: [Body]
                   , distanceScaleExponent :: Double
                   , origin :: (Double, Double)
                   , paused :: Bool
                   , track :: Maybe Int }

state :: State
state = State { planets = [sun, jupiter, earth, moon] 
              , distanceScaleExponent = -12
              , origin = (0, 0)
              , paused = False
              , track = Just 0 }

main :: IO ()
main = do
  (_progname, _) <- getArgsAndInitialize
  _window <- createWindow "Physix"
  currentTime <- getCurrentTime
  lastTime' <- newIORef $ utctDayTime currentTime
  state' <- newIORef state
  displayCallback $= display state'
  reshapeCallback $= Just reshape
  idleCallback $= Just (idle lastTime' state')
  keyboardMouseCallback $= Just (keyboardMouse state')
  mainLoop

display :: IORef (State) -> DisplayCallback
display state' = do
  clear [ ColorBuffer ]
  state <- get state'
  let scaleFactor = 10.0 ** (distanceScaleExponent state)
  mapM_ (draw (findOrigin state) scaleFactor) $ planets state
  flush
  where
    findOrigin State { origin = o, track = Nothing } = o
    findOrigin State { origin = (xo,yo), track = Just t, planets = p }
      | t >= 0 && t < length p = fromVector ( (Vec xo yo) |-| (pos (p !! t)))
      | otherwise              = fromVector ( (Vec xo yo) |-| (pos (p !! 0)))

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

idle :: IORef DiffTime -> IORef State -> IdleCallback
idle lastTime' state' = do
  state <- get state'
  lastTime <- get lastTime'
  time <- (getCurrentTime >>= (return . utctDayTime))
  let deltaTime = time - lastTime
  if deltaTime > deltaTimeResolution
    then do
      writeIORef lastTime' time 
      if paused state
        then return ()
        else writeIORef state' $ state { planets = simulate (planets state) (seconds deltaTime) }
    else return ()
  postRedisplay Nothing
  where
    seconds t = fromRational $ toRational t

keyboardMouse :: IORef State -> KeyboardMouseCallback
keyboardMouse state' key Down _ _ = do
  state <- get state'
  let scaleFactor = 10.0 ** (-(distanceScaleExponent state))
  case key of
    (Char '=') -> state' $~! (\s@State { distanceScaleExponent = e } -> s { distanceScaleExponent = e + 0.1 })
    (Char '-') -> state' $~! (\s@State { distanceScaleExponent = e } -> s { distanceScaleExponent = e - 0.1 })
    (SpecialKey KeyUp)    -> state' $~! (\s@State { origin = (x,y)} -> s { origin = (x, y + 0.1 * scaleFactor) })
    (SpecialKey KeyDown)  -> state' $~! (\s@State { origin = (x,y)} -> s { origin = (x, y - 0.1 * scaleFactor) })
    (SpecialKey KeyLeft)  -> state' $~! (\s@State { origin = (x,y)} -> s { origin = (x - 0.1 * scaleFactor,y) })
    (SpecialKey KeyRight) -> state' $~! (\s@State { origin = (x,y)} -> s { origin = (x + 0.1 * scaleFactor,y) })
    (Char ' ') -> state' $=! state { paused = not (paused state) }
    (Char num) | num >= '0' && num <= '9' -> state' $=! state { track = Just $ ord num - ord '0', origin = (0,0) }
    _ -> return ()
keyboardMouse _ _ _ _ _ = return ()