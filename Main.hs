module Main where

import Graphics.UI.GLUT
import Control.Monad
import Data.IORef
import Data.Time.Clock
import Data.List
import Physics
import Graphics

deltaTimeResolution = 0.01

world :: World
world = [ Particle { pos = Vec 50 50, vel = Vec 10 (-10), acc = Vec 0 0, rad = 5, mass = 1000000000000 }
        , Particle { pos = Vec 10 10,  vel = Vec 0 0, acc = Vec 0 0, rad = 5, mass = 500000000000000 }
        , Particle { pos = Vec 20 0, vel = Vec 0 20, acc = Vec 0 0, rad = 5, mass = 10000000000000 }
        ]

main :: IO ()
main = do
  (_progname, _) <- getArgsAndInitialize
  _window <- createWindow "Physix"
  currentTime <- getCurrentTime
  lastTime <- newIORef $ utctDayTime currentTime
  world' <- newIORef world
  displayCallback $= display world'
  reshapeCallback $= Just reshape
  idleCallback $= Just (idle lastTime world')
  keyboardMouseCallback $= Just keyboardMouse
  mainLoop

display :: IORef World -> DisplayCallback
display world = do
  clear [ ColorBuffer ]
  world' <- get world
  mapM_ draw $ world'
  flush

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  postRedisplay Nothing

idle :: IORef DiffTime -> IORef World -> IdleCallback
idle lastTime world = do
  world' <- get world
  lastTime' <- get lastTime
  time <- (getCurrentTime >>= (return . utctDayTime))
  let deltaTime = time - lastTime'
  if deltaTime > deltaTimeResolution
    then do
      writeIORef lastTime time 
      writeIORef world $ simulate world' $ seconds deltaTime
    else return ()
  postRedisplay Nothing
  where
    seconds t = fromRational $ toRational t

keyboardMouse :: KeyboardMouseCallback
keyboardMouse key Down _ _ = case key of
  --(SpecialKey KeyUp) -> 
  --(SpecialKey KeyDown) -> 
  _ -> return ()
keyboardMouse _ _ _ _ = return ()
