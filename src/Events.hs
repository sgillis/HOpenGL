module Events where

import Control.Concurrent.STM (TQueue, atomically, writeTQueue)
import qualified Graphics.UI.GLFW as G

import Model

setCallbacks :: TQueue Event -> G.Window -> IO ()
setCallbacks eventsChan win = do
    G.setErrorCallback (Just $ errorCallback eventsChan)
    G.setKeyCallback win (Just $ keyCallback eventsChan)

errorCallback   :: TQueue Event -> G.ErrorCallback
keyCallback     :: TQueue Event -> G.KeyCallback

errorCallback tc e s =
    atomically $ writeTQueue tc $ EventError e s
keyCallback tc win k sc action mods =
    atomically $ writeTQueue tc $ EventKey win k sc action mods
