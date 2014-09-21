module Graphics.ArrayObjects where

import Graphics.Rendering.OpenGL

makeArrayObject :: IO VertexArrayObject
makeArrayObject = genObjectName

withArrayObjectBound :: VertexArrayObject -> IO () -> IO ()
withArrayObjectBound vao m = do
    bindVertexArrayObject $= Just vao
    m
    bindVertexArrayObject $= Nothing
