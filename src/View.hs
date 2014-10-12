module View where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (gets)
import Linear
import Linear.OpenGL()
import Graphics.GLUtil.Camera3D
import qualified Graphics.Rendering.OpenGL as GL

import Model
import Graphics.Renderable

draw :: App
draw = do
    c <- gets cube
    p <- gets player
    w <- gets stateWindowWidth
    h <- gets stateWindowHeight

    liftIO $ do
        GL.clearColor GL.$= GL.Color4 0 0 0 1
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]

        renderWith c $ \program -> do
            cameraLoc <- GL.get $ GL.uniformLocation program "camera"
            GL.uniform cameraLoc GL.$= camMatrix (cam p)
            projectionLoc <- GL.get $ GL.uniformLocation program "projection"
            GL.uniform projectionLoc GL.$= projection w h
            modelLoc <- GL.get $ GL.uniformLocation program "model"
            GL.uniform modelLoc GL.$= transMatrix (V3 0 0 0)

        GL.flush

cam :: Player -> Camera GL.GLfloat
cam player =
    pan (realToFrac (-x/5)) $
    tilt (realToFrac (-y/5)) $
    dolly (v3toGL (position player)) $
    fpsCamera
    where x = horizontalAngle player
          y = verticalAngle player

transMatrix :: Position -> M44 GL.GLfloat
transMatrix pos =
    mkTransformation
        (axisAngle (V3 1 1 1) (realToFrac 0)) (v3toGL pos)

projection :: Int -> Int -> M44 GL.GLfloat
projection w h = projectionMatrix 45 (realToFrac w/realToFrac h) 0.01 100

v3toGL :: V3 Double -> V3 GL.GLfloat
v3toGL x = fmap realToFrac x
