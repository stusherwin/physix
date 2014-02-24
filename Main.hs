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

sun     = Planet { colour = T.Yellow, radius = 6.96e8, mass' = 1.98e30, distance = 0,       speed = 0 }
mercury = Planet { colour = T.Red,    radius = 2.43e6, mass' = 3.30e23, distance = 5.79e10, speed = 4.78e4 }
venus   = Planet { colour = T.Yellow, radius = 6.05e6, mass' = 4.86e24, distance = 1.08e11, speed = 3.50e4 }
earth   = Planet { colour = T.Blue,   radius = 6.37e6, mass' = 5.97e24, distance = 1.49e11, speed = 2.97e4 }
moon    = Planet { colour = T.White,  radius = 1.73e6, mass' = 7.34e22, 
                     distance = (distance earth) + 3.84e8, speed = (speed earth) + 1.02e3 }
mars    = Planet { colour = T.Red,    radius = 3.40e6, mass' = 6.42e23, distance = 2.27e11, speed = 2.40e4 }
jupiter = Planet { colour = T.Red,    radius = 6.99e7, mass' = 1.89e27, distance = 7.78e11, speed = 1.30e4 }
saturn  = Planet { colour = T.Yellow, radius = 6.03e7, mass' = 5.68e26, distance = 1.43e12, speed = 9.69e3 }

data Planet = Planet { colour   :: T.Colour
                     , radius   :: Double
                     , mass'    :: Double
                     , distance :: Double
                     , speed    :: Double   }

fromPlanet :: Planet -> Body
fromPlanet Planet { colour = c, radius = r, mass' = m, distance = d, speed = s }
  = Body { col = c, rad = r, mass = m, pos = Vec d 0, vel = Vec 0 s, acc = Vec 0 0 }

data State = State { planets               :: [Body]
                   , distanceScaleExponent :: Double
                   , origin                :: (Double, Double)
                   , paused                :: Bool
                   , track                 :: Maybe Int        }

state :: State
state = State { planets = map fromPlanet [sun, mercury, venus, earth, moon, mars, jupiter, saturn] 
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
    findOrigin State { origin = o,       track = Nothing } 
      = o
    findOrigin State { origin = (xo,yo), track = Just t, planets = p }
      = fromVector ( (Vec xo yo) |-| (pos (p !! t)))

reshape :: ReshapeCallback
reshape (Size w h) = do
  viewport $= (Position 0 0, Size w h)
  let wp = (fromIntegral w) / 300.0
  let hp = (fromIntegral h) / 300.0
  matrixMode $= Projection
  loadIdentity
  ortho2D (-wp) wp (-hp) hp
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
    (SpecialKey KeyUp)    -> state' $~! (\s@State { origin = (x,y)} -> s { origin = (x, y + 0.1 * scaleFactor) })
    (SpecialKey KeyDown)  -> state' $~! (\s@State { origin = (x,y)} -> s { origin = (x, y - 0.1 * scaleFactor) })
    (SpecialKey KeyLeft)  -> state' $~! (\s@State { origin = (x,y)} -> s { origin = (x - 0.1 * scaleFactor,y) })
    (SpecialKey KeyRight) -> state' $~! (\s@State { origin = (x,y)} -> s { origin = (x + 0.1 * scaleFactor,y) })
    (Char '=') -> state' $~! (\s@State { distanceScaleExponent = e } -> s { distanceScaleExponent = e + 0.1 })
    (Char '-') -> state' $~! (\s@State { distanceScaleExponent = e } -> s { distanceScaleExponent = e - 0.1 })
    (Char ' ') -> state' $=! state { paused = not (paused state) }
    (Char num) | num >= '0' && num <= '9' && trackPlanet < length (planets state) 
      -> state' $=! state { track = Just trackPlanet, origin = (0,0) }
        where trackPlanet = ord num - ord '0'
    _ -> return ()
keyboardMouse _ _ _ _ _ = return ()