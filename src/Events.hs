module Events where

import Control.Concurrent.STM (TQueue, atomically, writeTQueue)
import qualified Graphics.UI.GLFW as G

import Model

setCallbacks :: TQueue Event -> G.Window -> IO ()
setCallbacks eventsChan win = do
    G.setErrorCallback (Just $ errorCallback eventsChan)
    G.setKeyCallback win (Just $ keyCallback eventsChan)
    G.setCursorPosCallback win (Just $ cursorPosCallback eventsChan)

errorCallback     :: TQueue Event -> G.ErrorCallback
keyCallback       :: TQueue Event -> G.KeyCallback
cursorPosCallback :: TQueue Event -> G.CursorPosCallback

errorCallback tc e s =
    atomically $ writeTQueue tc $ EventError e s
keyCallback tc win k sc action mods =
    atomically $ writeTQueue tc $ EventKey win k sc action mods
cursorPosCallback tc win x y =
    atomically $ writeTQueue tc $ EventCursor x y
