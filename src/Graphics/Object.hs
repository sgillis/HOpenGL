module Graphics.Object (makeObject, Object) where

import Graphics.Rendering.OpenGL
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.IO.Class (liftIO)
import Data.Word (Word16)

import Graphics.Shaders (makeProgram)
import Graphics.Buffers (makeBufferWithData, ptrOffset)
import Graphics.ArrayObjects (makeArrayObject)
import Graphics.Renderable

data Object = Object Program [BufferObject] NumArrayIndices
              PrimitiveMode VertexArrayObject

enableVertexArray :: Program -> (String, VertexArrayDescriptor a) -> IO ()
enableVertexArray program (name, descriptor) = do
    location <- get $ attribLocation program name
    vertexAttribArray location $= Enabled
    vertexAttribPointer location $= (ToFloat, descriptor)

makeObject :: [(ShaderType, FilePath)] -> [Float] -> [Word16] ->
              [(String, VertexArrayDescriptor a)] -> PrimitiveMode ->
              NumArrayIndices -> MaybeT IO Object
makeObject programSpec vertices indices attribs mode num = do
    program <- makeProgram programSpec
    vertexBuffer <- liftIO $ makeBufferWithData ArrayBuffer vertices StaticDraw
    indexBuffer <- liftIO $ makeBufferWithData ElementArrayBuffer indices StaticDraw
    vao <- liftIO makeArrayObject

    liftIO $ do
        bindVertexArrayObject $= Just vao
        bindBuffer ArrayBuffer $= Just vertexBuffer
        bindBuffer ElementArrayBuffer $= Just indexBuffer
        mapM_ (enableVertexArray program) attribs
        bindVertexArrayObject $= Nothing
        bindBuffer ArrayBuffer $= Nothing
        bindBuffer ElementArrayBuffer $= Nothing
    return $ Object program [vertexBuffer, indexBuffer] num mode vao

instance Renderable Object where
    render object = renderWith object (\_ -> return ())

    renderWith (Object program _ size mode vao) m = do
        bindVertexArrayObject $= Just vao
        currentProgram $= Just program
        m program
        drawElements mode size UnsignedShort (ptrOffset 0)
        bindVertexArrayObject $= Nothing
        currentProgram $= Nothing
