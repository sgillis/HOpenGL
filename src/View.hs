module View where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (gets)
import Control.Monad (forM_)
import Linear
import Linear.OpenGL()
import Graphics.GLUtil.Camera3D
import qualified Graphics.Rendering.OpenGL as GL

import Model
import Utils
import Graphics.Renderable
import Graphics.Object

draw :: App
draw = do
    c <- gets cube
    cubes <- gets cubePositions
    p <- gets player
    w <- gets stateWindowWidth
    h <- gets stateWindowHeight

    liftIO $ do
        GL.clearColor GL.$= GL.Color4 0 0 0 1
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]

        forM_ cubes (drawCube w h p c)

        GL.flush

drawCube :: Int -> Int -> Player -> Object -> M44 GL.GLfloat -> IO ()
drawCube w h player cube modelmatrix =
    renderWith cube $ \program -> do
        cameraLoc <- GL.get $ GL.uniformLocation program "camera"
        GL.uniform cameraLoc GL.$= camMatrix (cam player)
        projectionLoc <- GL.get $ GL.uniformLocation program "projection"
        GL.uniform projectionLoc GL.$= projection w h
        modelLoc <- GL.get $ GL.uniformLocation program "model"
        GL.uniform modelLoc GL.$= modelmatrix


cam :: Player -> Camera GL.GLfloat
cam player =
    panRad (realToFrac x) $
    tiltRad (realToFrac y) $
    dolly (v3toGL (position player)) $
    fpsCamera
    where x = horizontalAngle player
          y = verticalAngle player

projection :: Int -> Int -> M44 GL.GLfloat
projection w h = projectionMatrix 45 (realToFrac w/realToFrac h) 0.01 100
