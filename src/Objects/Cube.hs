module Objects.Cube (makeCube) where

import Graphics.Rendering.OpenGL
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.IO.Class (liftIO)

import Paths_HOpenGL
import Graphics.Object (makeObject, Object)
import Graphics.Buffers (ptrOffset)

makeCube :: MaybeT IO Object
makeCube = do
    vertShaderPath <- liftIO $ getDataFileName "vertex.shader"
    fragShaderPath <- liftIO $ getDataFileName "fragment.shader"
    makeObject
        [ (VertexShader, vertShaderPath)
        , (FragmentShader, fragShaderPath)
        ] vertexPositions indices
        [("position", VertexArrayDescriptor 4 Float 0 $ ptrOffset 0)]
        Triangles 36
    where
        vertexPositions = 
            [  0.5,  0.5,  0.5, 1.0
            ,  0.5, -0.5,  0.5, 1.0
            , -0.5, -0.5,  0.5, 1.0
            , -0.5,  0.5,  0.5, 1.0
            ,  0.5,  0.5, -0.5, 1.0
            ,  0.5, -0.5, -0.5, 1.0
            , -0.5, -0.5, -0.5, 1.0
            , -0.5,  0.5, -0.5, 1.0
            ]
        indices =
            [ 0, 1, 2
            , 0, 2, 3
            , 0, 4, 5
            , 0, 5, 1
            , 4, 7, 6
            , 4, 6, 5
            , 3, 2, 6
            , 3, 6, 7
            , 1, 5, 2
            , 1, 6, 2
            , 7, 4, 0
            , 7, 0, 3
            ]
