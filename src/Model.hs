module Model where

import Control.Concurrent.STM (TQueue)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State.Strict (StateT)

import qualified Graphics.UI.GLFW as G

import Graphics.Object

data Env = Env
    { envEventsChan     :: TQueue Event
    , envWindow         :: !G.Window
    }

data State = State
    { stateWindowWidth  :: !Int
    , stateWindowHeight :: !Int
    , cube              :: Object
    , xpos              :: !Double
    , ypos              :: !Double
    , zpos              :: !Double
    }

data Event =
      EventError !G.Error !String
    | EventKey   !G.Window !G.Key !Int !G.KeyState !G.ModifierKeys
    deriving Show

type RST r st m = ReaderT r (StateT st m)

type App = RST Env State IO ()
