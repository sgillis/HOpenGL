module View where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (gets)
import qualified Graphics.Rendering.OpenGL as GL

import Model
import Graphics.Renderable

draw :: App
draw = do
    triangle <- gets triangle
    liftIO $ do
        GL.clearColor GL.$= GL.Color4 0 0 0 1
        GL.clear [GL.ColorBuffer]

        render triangle

        GL.flush
