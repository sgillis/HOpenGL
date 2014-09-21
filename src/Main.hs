module Main (main) where

-------------------------------------------------------------------------------

import Control.Concurrent.STM (TQueue, newTQueueIO, tryReadTQueue, atomically)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void, unless, when)
import Control.Monad.Reader (runReaderT, asks)
import Control.Monad.State.Strict (runStateT)

import qualified Graphics.UI.GLFW as G

import Model
import Events
import View
import Window (withWindow)
import Objects.Triangle (makeTriangle)

-------------------------------------------------------------------------------

runRST :: Monad m => RST r st m a -> r -> st -> m (a,st)
runRST rst r st = flip runStateT st . flip runReaderT r $ rst

runApp :: Env -> State -> IO ()
runApp env state = void $ runRST run env state

-------------------------------------------------------------------------------

main :: IO ()
main = do
    let width  = 640
        height = 480

    eventsChan <- newTQueueIO :: IO (TQueue Event)

    withWindow width height "test" $ \win -> do
        setCallbacks eventsChan win
        G.swapInterval 1

        (fbWidth, fbHeight) <- G.getFramebufferSize win
        
        mtriangle <- runMaybeT makeTriangle

        case mtriangle of
            Nothing -> putStrLn "Failed to load objects"
            Just triangle -> do
                let env = Env
                        { envEventsChan     = eventsChan
                        , envWindow         = win
                        }
                    state = State
                        { stateWindowWidth  = fbWidth
                        , stateWindowHeight = fbHeight
                        , triangle          = triangle
                        }
                runApp env state
                
-------------------------------------------------------------------------------

run :: App
run = do
    win <- asks envWindow

    draw
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
        (EventError e s) -> liftIO $ G.setWindowShouldClose win True
        (EventKey win k sc action mods) ->
            when (k == G.Key'Escape && action == G.KeyState'Pressed) $
                liftIO $ G.setWindowShouldClose win True
