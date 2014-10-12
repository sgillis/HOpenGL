module Model where

import Control.Concurrent.STM (TQueue)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State.Strict (StateT)
import Linear

import qualified Graphics.UI.GLFW as G
import qualified Graphics.Rendering.OpenGL as GL

import Graphics.Object
import Utils

data Env = Env
    { envEventsChan     :: TQueue Event
    , envWindow         :: !G.Window
    }

data State = State
    { stateWindowWidth  :: !Int
    , stateWindowHeight :: !Int
    , cube              :: Object
    , cubePositions     :: [M44 GL.GLfloat]
    , player            :: Player
    }

data Event =
      EventError  !G.Error !String
    | EventKey    !G.Window !G.Key !Int !G.KeyState !G.ModifierKeys
    | EventCursor !Double !Double
    deriving Show

type RST r st m = ReaderT r (StateT st m)

type App = RST Env State IO ()

type Position       = V3 Double
type Velocity       = V3 Double

data Player = Player
    { position          :: Position
    , velocity          :: Velocity
    , horizontalAngle   :: Double
    , verticalAngle     :: Double
    }

initialPlayer :: Player
initialPlayer = 
    Player { position = V3 0 2 5
           , velocity = V3 0 0 0
           , horizontalAngle = 0
           , verticalAngle = 0
           }

direction :: Player -> V3 Double
direction player = 
    let h = horizontalAngle player + (-pi/2)
        v = verticalAngle player
    in V3 (cos h) (sin v) (-sin h)

initialCubes :: [M44 GL.GLfloat]
initialCubes =
    [ transMatrix (V3 0 0 0)
    , transMatrix (V3 1 0 0)
    ]
