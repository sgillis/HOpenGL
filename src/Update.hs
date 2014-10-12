module Update (update) where

import Control.Concurrent.STM (tryReadTQueue, atomically)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Control.Monad.State.Strict (modify)
import Linear

import qualified Graphics.UI.GLFW as G

import Model

import Debug.Trace

update :: App
update = processEvents

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
        (EventKey _ G.Key'W _ _ _)         -> movePlayer (V3     0      0  (-0.2))
        (EventKey _ G.Key'S _ _ _)         -> movePlayer (V3     0      0    0.2 )
        (EventKey _ G.Key'D _ _ _)         -> movePlayer (V3   0.2      0      0 )
        (EventKey _ G.Key'A _ _ _)         -> movePlayer (V3 (-0.2)     0      0 )
        (EventKey _ G.Key'Space _ _ _)     -> movePlayer (V3     0    0.2      0 )
        (EventKey _ G.Key'LeftShift _ _ _) -> movePlayer (V3     0  (-0.2)     0 )
        (EventCursor x y) -> setPlayerDirection x y
        _ -> return ()

movePlayer :: V3 Double -> App
movePlayer direction =
    modify $ \s -> s { player = move direction $ player s }

move :: V3 Double -> Player -> Player
move direction player =
    player { position = position player + direction }

setPlayerDirection :: Double -> Double -> App
setPlayerDirection x y =
    modify $ \s -> s { player = setDirection x y $ player s }

setDirection :: Double -> Double -> Player -> Player
setDirection x y player =
    player { horizontalAngle = x
           , verticalAngle   = y
           }
