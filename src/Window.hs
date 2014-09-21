module Window (withWindow) where

import System.Exit
import qualified Graphics.UI.GLFW as G

withWindow :: Int -> Int -> String -> (G.Window -> IO ()) -> IO ()
withWindow width height title f = do
    G.setErrorCallback $ Just simpleErrorCallback
    successfulInit <- G.init
    bool successfulInit exitFailure $ do
        mw <- G.createWindow width height title Nothing Nothing
        case mw of
            Nothing -> G.terminate >> exitFailure
            Just win -> do
                G.makeContextCurrent mw
                f win
                G.destroyWindow win
                G.terminate
                exitSuccess

bool :: Bool -> a -> a -> a
bool b falseRes trueRes = if b then trueRes else falseRes

simpleErrorCallback :: G.ErrorCallback
simpleErrorCallback err = putStrLn
