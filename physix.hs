import Graphics.UI.GLUT
import Control.Monad
import Data.IORef
import Data.Time.Clock
import Data.List

deltaTimeResolution = 0.01
granularity = 0.00001
gravitationalConstant = 6.67384e-11

class Vector a where
  (|+|) :: a -> a -> a
  (|-|) :: a -> a -> a
  (|*|) :: a -> Double -> a
  (|/|) :: a -> Double -> a
  unit :: a -> a
  absSquare :: a -> Double
  vecSum :: [a] -> a

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

class Drawable a where
  draw :: a -> IO()

type World = [Particle]
data Particle = Particle { pos :: Vec, vel :: Vec, acc :: Vec, rad :: Double, mass :: Double } 
  deriving (Eq, Show)

instance Drawable Particle where
  draw Particle { pos = Vec x y, rad = r } = circle 10 (realToFrac r) (Color3 1 0 0) (realToFrac x, realToFrac y)

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

simulate :: [Particle] -> Double -> [Particle]
simulate particles timePeriod = turnCrank particles timePeriod
  where turnCrank ps t | t <= 0    = ps
                       | otherwise = turnCrank (updateDynamics ps granularity) (t - granularity)

updateDynamics :: [Particle] -> Double -> [Particle]
updateDynamics particles dt = zipWith4 update particles as vs xs
  where 
    as = gravitationalField $ map (\p -> (pos p, mass p)) particles
    vs = zipWith (\a v -> v |+| (a |*| dt)) as $ map vel particles
    xs = zipWith (\v x -> x |+| (v |*| dt)) vs $ map pos particles
    update p a v x = Particle { rad = rad p, mass = mass p, pos = x, vel = v, acc = a }

gravitationalField :: [(Vec,Double)] -> [Vec]
gravitationalField particles = map (\(p,m) -> fieldAt p $ delete (p,m) particles) particles
  where
    fieldAt posj is = (vecSum $ map (fieldContribution posj) is ) |*| (-gravitationalConstant)
    fieldContribution posj (posi, mi) = (unit $ posj |-| posi) |*| mi |/| absSquare (posi |-| posj)

keyboardMouse :: KeyboardMouseCallback
keyboardMouse key Down _ _ = case key of
  --(SpecialKey KeyUp) -> 
  --(SpecialKey KeyDown) -> 
  _ -> return ()
keyboardMouse _ _ _ _ = return ()

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