module Main (main) where

-------------------------------------------------------------------------------

import Control.Concurrent.STM (TQueue, newTQueueIO, tryReadTQueue, atomically)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void, unless)
import Control.Monad.Reader (runReaderT, asks)
import Control.Monad.State.Strict (runStateT, modify)

import qualified Graphics.UI.GLFW as G

import Model
import Events
import View
import Window (withWindow)
import Objects.Cube (makeCube)
import Objects.Triangle (makeTriangle)

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
                        , xpos              = 0
                        , ypos              = 0
                        , zpos              = (-2)
                        }
                runApp env state
                
-------------------------------------------------------------------------------

run :: App
run = do
    win <- asks envWindow

    time <- liftIO G.getTime
    draw time
    liftIO $ do
        G.swapBuffers win
        G.pollEvents
    processEvents

    q <- liftIO $ G.windowShouldClose win
    unless q run

processEvents :: App
processEvents = do
    tc <- asks envEventsChan
    me <- liftIO $ atomically $ tryReadTQueue tc
    case me of
        Nothing -> return ()
        Just e -> do
            processEvent e
            processEvents

processEvent :: Event -> App
processEvent e = do
    win <- asks envWindow
    case e of
        (EventError _ _) -> liftIO $ G.setWindowShouldClose win True
        (EventKey _ G.Key'Escape _ G.KeyState'Pressed _) ->
            liftIO $ G.setWindowShouldClose win True
        (EventKey _ G.Key'W _ _ _) ->
            modify $ \s -> s { ypos = ypos s + 0.2 }
        (EventKey _ G.Key'S _ _ _) ->
            modify $ \s -> s { ypos = ypos s - 0.2 }
        (EventKey _ G.Key'D _ _ _) ->
            modify $ \s -> s { xpos = xpos s + 0.2 }
        (EventKey _ G.Key'A _ _ _) ->
            modify $ \s -> s { xpos = xpos s - 0.2 }
        (EventKey _ G.Key'R _ _ _) ->
            modify $ \s -> s { zpos = zpos s + 0.2 }
        (EventKey _ G.Key'F _ _ _) ->
            modify $ \s -> s { zpos = zpos s - 0.2 }
        _ -> return ()
