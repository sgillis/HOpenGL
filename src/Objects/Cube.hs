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
        ] vertexPositions
        [("position", VertexArrayDescriptor 4 Float 0 $ ptrOffset 0)
        ,("normal", VertexArrayDescriptor 4 Float 0 $ ptrOffset (6*6*4*4))
        ]
        Triangles 36
    where
        vertexPositions = 
            -- Vertices
            -- Front face
            [  0.5,  0.5,  0.5, 1.0  -- 0
            ,  0.5, -0.5,  0.5, 1.0  -- 1
            , -0.5, -0.5,  0.5, 1.0  -- 2
            ,  0.5,  0.5,  0.5, 1.0  -- 0
            , -0.5, -0.5,  0.5, 1.0  -- 2
            , -0.5,  0.5,  0.5, 1.0  -- 3
            -- Right face
            ,  0.5,  0.5,  0.5, 1.0  -- 0
            ,  0.5,  0.5, -0.5, 1.0  -- 4
            ,  0.5, -0.5, -0.5, 1.0  -- 5
            ,  0.5,  0.5,  0.5, 1.0  -- 0
            ,  0.5, -0.5, -0.5, 1.0  -- 5
            ,  0.5, -0.5,  0.5, 1.0  -- 1
            -- Back face
            ,  0.5,  0.5, -0.5, 1.0  -- 4
            , -0.5,  0.5, -0.5, 1.0  -- 7
            , -0.5, -0.5, -0.5, 1.0  -- 6
            ,  0.5,  0.5, -0.5, 1.0  -- 4
            , -0.5, -0.5, -0.5, 1.0  -- 6
            ,  0.5, -0.5, -0.5, 1.0  -- 5
            -- Left face
            , -0.5,  0.5,  0.5, 1.0  -- 3
            , -0.5, -0.5,  0.5, 1.0  -- 2
            , -0.5, -0.5, -0.5, 1.0  -- 6
            , -0.5,  0.5,  0.5, 1.0  -- 3
            , -0.5, -0.5, -0.5, 1.0  -- 6
            , -0.5,  0.5, -0.5, 1.0  -- 7
            -- Bottom face
            ,  0.5, -0.5,  0.5, 1.0  -- 1
            ,  0.5, -0.5, -0.5, 1.0  -- 5
            , -0.5, -0.5,  0.5, 1.0  -- 2
            ,  0.5, -0.5, -0.5, 1.0  -- 5
            , -0.5, -0.5, -0.5, 1.0  -- 6
            , -0.5, -0.5,  0.5, 1.0  -- 2
            -- Top face
            , -0.5,  0.5, -0.5, 1.0  -- 7
            ,  0.5,  0.5, -0.5, 1.0  -- 4
            ,  0.5,  0.5,  0.5, 1.0  -- 0
            , -0.5,  0.5, -0.5, 1.0  -- 7
            ,  0.5,  0.5,  0.5, 1.0  -- 0
            , -0.5,  0.5,  0.5, 1.0  -- 3
            -- Normals
            -- Front face
            ,  0.0,  0.0,  1.0, 1.0
            ,  0.0,  0.0,  1.0, 1.0
            ,  0.0,  0.0,  1.0, 1.0
            ,  0.0,  0.0,  1.0, 1.0
            ,  0.0,  0.0,  1.0, 1.0
            ,  0.0,  0.0,  1.0, 1.0
            -- Right face
            ,  1.0,  0.0,  0.0, 1.0
            ,  1.0,  0.0,  0.0, 1.0
            ,  1.0,  0.0,  0.0, 1.0
            ,  1.0,  0.0,  0.0, 1.0
            ,  1.0,  0.0,  0.0, 1.0
            ,  1.0,  0.0,  0.0, 1.0
            -- Back face
            ,  0.0,  0.0, -1.0, 1.0
            ,  0.0,  0.0, -1.0, 1.0
            ,  0.0,  0.0, -1.0, 1.0
            ,  0.0,  0.0, -1.0, 1.0
            ,  0.0,  0.0, -1.0, 1.0
            ,  0.0,  0.0, -1.0, 1.0
            -- Left face
            , -1.0,  0.0,  0.0, 1.0
            , -1.0,  0.0,  0.0, 1.0
            , -1.0,  0.0,  0.0, 1.0
            , -1.0,  0.0,  0.0, 1.0
            , -1.0,  0.0,  0.0, 1.0
            , -1.0,  0.0,  0.0, 1.0
            -- Bottom face
            ,  0.0, -1.0,  0.0, 1.0
            ,  0.0, -1.0,  0.0, 1.0
            ,  0.0, -1.0,  0.0, 1.0
            ,  0.0, -1.0,  0.0, 1.0
            ,  0.0, -1.0,  0.0, 1.0
            ,  0.0, -1.0,  0.0, 1.0
            -- Top face
            ,  0.0,  1.0,  0.0, 1.0
            ,  0.0,  1.0,  0.0, 1.0
            ,  0.0,  1.0,  0.0, 1.0
            ,  0.0,  1.0,  0.0, 1.0
            ,  0.0,  1.0,  0.0, 1.0
            ,  0.0,  1.0,  0.0, 1.0
            ]
