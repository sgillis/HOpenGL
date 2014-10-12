module Update (update) where

import Control.Concurrent.STM (tryReadTQueue, atomically)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Control.Monad.State.Strict (modify, gets)
import Linear

import qualified Graphics.UI.GLFW as G

import Model

-------------------------------------------------------------------------------

update :: App
update = processEvents >> physics

-------------------------------------------------------------------------------

physics :: App
physics =
    modify $ \s -> s { player = playerPhysics $ player s }

playerPhysics :: Player -> Player
playerPhysics player =
    let position' = (position player) ^+^ (0.01 *^ velocity player)
        velocity' = (velocity player) ^-^ (0.05 *^ velocity player)
    in player { position = position'
              , velocity = velocity' }

-------------------------------------------------------------------------------

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
        (EventKey _ G.Key'S _ _ _)         -> movePlayer (V3   0    0   1 )
        (EventKey _ G.Key'W _ _ _)         -> movePlayer (V3   0    0 (-1))
        (EventKey _ G.Key'A _ _ _)         -> movePlayer (V3   1    0   0 )
        (EventKey _ G.Key'D _ _ _)         -> movePlayer (V3 (-1)   0   0 )
        (EventKey _ G.Key'Space _ _ _)     -> movePlayer (V3   0    1   0 )
        (EventKey _ G.Key'LeftShift _ _ _) -> movePlayer (V3   0  (-1)  0 )
        (EventCursor x y) -> setPlayerDirection x y
        _ -> return ()

-------------------------------------------------------------------------------

movePlayer :: V3 Double -> App
movePlayer input = do
    p <- gets player
    modify $ \s -> s { player = move input $ player s }

move :: V3 Double -> Player -> Player
move input player =
    let direction = moveDirection input player
    in  player { velocity = velocity player + direction }

moveDirection :: V3 Double -> Player -> V3 Double
moveDirection (V3 x _ z) player =
    let moveDir = normalize $ flatten $ direction player
        strafeDir = strafeDirection moveDir
        move = z *^ moveDir
        strafe = x *^ strafeDir
    in move + strafe

strafeDirection :: V3 Double -> V3 Double
strafeDirection direction =
    let quaternion = axisAngle (V3 0 1 0) (-pi/2)
        rotMat = fromQuaternion quaternion
    in rotMat !* direction

flatten :: V3 Double -> V3 Double
flatten (V3 x _ z) = V3 x 0 z

setPlayerDirection :: Double -> Double -> App
setPlayerDirection x y = do
    modify $ \s -> s { player = setDirection x y $ player s }

setDirection :: Double -> Double -> Player -> Player
setDirection x y player =
    player { horizontalAngle = (-x) / 1000
           , verticalAngle   = (-y) / 1000
           }

-------------------------------------------------------------------------------
