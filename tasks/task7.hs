import Graphics.UI.GLUT
import Data.IORef
import System.Random
import Control.Monad (replicateM)

data Sphere = Sphere {
  spherePosition :: (GLfloat, GLfloat),
  sphereVelocity :: (GLfloat, GLfloat),
  sphereSize :: GLfloat,
  sphereShade :: Color3 GLfloat
}

generateSphere :: GLfloat -> IO Sphere
generateSphere diameter = do
  posX <- randomRIO (-1.0, 1.0)
  posY <- randomRIO (-1.0, 1.0)
  velX <- randomRIO (-0.1, 0.1)
  velY <- randomRIO (-0.1, 0.1)
  red <- randomRIO (0.0, 1.0)
  green <- randomRIO (0.0, 1.0)
  blue <- randomRIO (0.0, 1.0)
  threadDelay 100000
  return $ Sphere (posX, posY) (velX, velY) diameter (Color3 red green blue)

renderDisplay :: IORef [Sphere] -> DisplayCallback
renderDisplay sphereRef = do
  spheres <- readIORef sphereRef
  clear [ColorBuffer]
  replicateM_ 5 $ mapM_ paintSphere spheres
  flush
  where
    paintSphere (Sphere (posX, posY) _ diameter color) = do
      preservingMatrix $ do
        translate $ Vector3 posX posY 0
        color color
        renderObject Solid $ Sphere' diameter 32 32

moveSpheres :: IORef [Sphere] -> IO ()
moveSpheres sphereRef = do
  spheres <- readIORef sphereRef
  let moveSphere (Sphere (posX, posY) (velX, velY) diameter shade) =
        let (newPosX, newPosY) = (posX + velX, posY + velY)
            (newVelX, newVelY) = if abs newPosX > 1.0 - diameter 
                                 then (-velX * 0.9, velY * 0.9) -- Slowing down
                                 else (velX, velY)
                                if abs newPosY > 1.0 - diameter 
                                 then (velX * 0.9, -velY * 0.9) -- Slowing down
                                 else (velX, velY)
        in Sphere (newPosX, newPosY) (newVelX, newVelY) diameter shade
  writeIORef sphereRef $ map moveSphere spheres

adjustViewport :: ReshapeCallback
adjustViewport newSize = do
  viewport $= (Position 0 0, newSize)
  matrixMode $= Projection
  loadIdentity
  perspective 45 (fromIntegral (width newSize) / fromIntegral (height newSize)) 0.1 100.0
  matrixMode $= Modelview 0

refreshScene :: IORef [Sphere] -> IdleCallback
refreshScene sphereRef = do
  moveSpheres sphereRef
  postRedisplay Nothing

main :: IO ()
main = do
  (_programName, _arguments) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  initialWindowSize $= Size 640 480
  initialWindowPosition $= Position 100 100
  _mainWindow <- createWindow "Bouncing Spheres"
  sphereRef <- newIORef []
  forM_ [1..10] $ \_ -> do
    sphere <- generateSphere 0.1
    modifyIORef sphereRef (sphere:)
  displayCallback $= renderDisplay sphereRef
  reshapeCallback $= Just adjustViewport
  idleCallback $= Just (refreshScene sphereRef)
  clearColor $= Color4 0 0 0 0
  mainLoop