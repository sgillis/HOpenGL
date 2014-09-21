module Objects.Triangle (makeTriangle) where

import Graphics.Rendering.OpenGL
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.IO.Class (liftIO)

import Paths_HOpenGL
import Graphics.Object (makeObject, Object)
import Graphics.Buffers (ptrOffset)

makeTriangle :: MaybeT IO Object
makeTriangle = do
    vertShaderPath <- liftIO $ getDataFileName "vertex.shader"
    fragShaderPath <- liftIO $ getDataFileName "fragment.shader"
    makeObject
        [ (VertexShader, vertShaderPath)
        , (FragmentShader, fragShaderPath)
        ] vertexPositions indices
        [("position", VertexArrayDescriptor 4 Float 0 $ ptrOffset 0)]
        Triangles 3
    where
        vertexPositions = 
            [ -0.5, -0.5, 0.0, 1.0
            ,  0.0,  0.5, 0.0, 1.0
            ,  0.5, -0.5, 0.0, 1.0
            ]
        indices =
            [ 0, 1, 2
            ]
