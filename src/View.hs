module View where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (gets)
import Linear
import Linear.OpenGL()
import Graphics.GLUtil.Camera3D
import qualified Graphics.Rendering.OpenGL as GL
import Data.Maybe

import Model
import Graphics.Renderable

transMatrix :: Double -> Double -> Double -> Double -> M44 GL.GLfloat
transMatrix t tx ty tz =
    mkTransformation
        (axisAngle (V3 1 1 1) (realToFrac t * 0))
        (V3 x y z)
    where x = realToFrac tx
          y = realToFrac ty
          z = realToFrac tz

cam :: Camera GL.GLfloat
cam = dolly (V3 0 0 0) fpsCamera

projection :: Int -> Int -> M44 GL.GLfloat
projection w h = projectionMatrix 45 (realToFrac w/realToFrac h) 0.01 100

draw :: Maybe Double -> App
draw mtime = do
    cube <- gets cube
    x <- gets xpos
    y <- gets ypos
    z <- gets zpos
    w <- gets stateWindowWidth
    h <- gets stateWindowHeight
    liftIO $ do
        GL.clearColor GL.$= GL.Color4 0 0 0 1
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
        GL.depthFunc GL.$= Just GL.Less
        GL.cullFace GL.$= Just GL.Front

        renderWith cube $ \program -> do
            cameraLoc <- GL.get $ GL.uniformLocation program "camera"
            GL.uniform cameraLoc GL.$= camMatrix cam
            projectionLoc <- GL.get $ GL.uniformLocation program "projection"
            GL.uniform projectionLoc GL.$= projection w h
            modelLoc <- GL.get $ GL.uniformLocation program "model"
            GL.uniform modelLoc GL.$= transMatrix (fromMaybe 0 mtime) x y z

        GL.flush
