module Utils (v3toGL, transMatrix) where

import Linear
import qualified Graphics.Rendering.OpenGL as GL

v3toGL :: V3 Double -> V3 GL.GLfloat
v3toGL x = fmap realToFrac x

transMatrix :: V3 Double -> M44 GL.GLfloat
transMatrix pos =
    mkTransformation
        (axisAngle (V3 1 1 1) (realToFrac 0)) (v3toGL pos)
