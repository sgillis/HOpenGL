module Graphics.Shaders (makeProgram) where

import Graphics.Rendering.OpenGL
import Control.Monad (guard, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT)
import Data.ByteString as BS (readFile, ByteString)

makeShader :: ShaderType -> ByteString -> MaybeT IO Shader
makeShader shaderType shaderSource = do
    shader <- lift $ createShader shaderType
    lift $
        shaderSourceBS shader $= shaderSource >>
        compileShader shader
    status <- lift $ get $ compileStatus shader
    unless status $ do
        infoLog <- lift $ get $ shaderInfoLog shader
        lift $
            putStrLn ("Failed compiling shader:\n" ++ infoLog) >>
            deleteObjectName shader
    guard status
    return shader

loadShader :: (ShaderType, FilePath) -> MaybeT IO Shader
loadShader (shaderType, filePath) =
    lift (BS.readFile filePath) >>= \shaderSource ->
        makeShader shaderType shaderSource

loadAndAttachShader :: Program -> (ShaderType, FilePath) -> MaybeT IO ()
loadAndAttachShader program shaderInfo =
    loadShader shaderInfo >>= \shader ->
        lift $ attachShader program shader

makeProgram :: [(ShaderType, FilePath)] -> MaybeT IO Program
makeProgram shaderList = do
    program <- lift createProgram
    shaders <- mapM loadShader shaderList
    lift $
        mapM_ (attachShader program) shaders >>
        linkProgram program >>
        mapM_ (detachShader program) shaders >>
        deleteObjectNames shaders
    status <- lift $ get $ linkStatus program
    unless status $ do
        infoLog <- lift $ get $ programInfoLog program
        lift $
            putStrLn ("Failed linking program:\n" ++ infoLog) >>
            deleteObjectName program
    guard status
    return program
