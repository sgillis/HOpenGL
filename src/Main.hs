module Main (main) where

-------------------------------------------------------------------------------

import Control.Concurrent.STM (TQueue, newTQueueIO)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void, unless)
import Control.Monad.Reader (runReaderT, asks)
import Control.Monad.State.Strict (runStateT)

import qualified Graphics.UI.GLFW as G
import qualified Graphics.Rendering.OpenGL as GL

import Model
import Events
import View
import Update
import Window (withWindow)
import Objects.Cube (makeCube)

-------------------------------------------------------------------------------

runRST :: Monad m => RST r st m a -> r -> st -> m (a,st)
runRST rst r st = flip runStateT st . flip runReaderT r $ rst

runApp :: Env -> State -> IO ()
runApp env state = void $ runRST run env state

-------------------------------------------------------------------------------

main :: IO ()
main = do
    let width  = 1280
        height = 720

    eventsChan <- newTQueueIO :: IO (TQueue Event)

    withWindow width height "test" $ \win -> do
        setCallbacks eventsChan win
        G.swapInterval 1
        G.setCursorInputMode win G.CursorInputMode'Disabled
        GL.depthFunc GL.$= Just GL.Less
        GL.cullFace GL.$= Just GL.Front

        (fbWidth, fbHeight) <- G.getFramebufferSize win
        
        mcube <- runMaybeT makeCube

        case mcube of
            Nothing -> putStrLn "Failed to load objects"
            Just c -> do
                let env = Env
                        { envEventsChan     = eventsChan
                        , envWindow         = win
                        }
                    state = State
                        { stateWindowWidth  = fbWidth
                        , stateWindowHeight = fbHeight
                        , cube              = c
                        , player            = initialPlayer
                        }
                runApp env state
                
-------------------------------------------------------------------------------

run :: App
run = do
    win <- asks envWindow

    update
    draw

    liftIO $ do
        G.swapBuffers win
        G.pollEvents

    q <- liftIO $ G.windowShouldClose win
    unless q run
