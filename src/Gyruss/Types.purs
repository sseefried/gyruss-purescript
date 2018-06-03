module Gyruss.Types where

import Prelude

-- import Audio.WebAudio.Types
import Control.Monad.Eff
import Control.Monad.Eff.Random
import Data.Maybe (Maybe)
import Data.List
import Data.List.Lazy as LL
import Data.Tuple
import Graphics.Canvas
import Math (pi)


-- For 3D projection purposes screenDist is the
-- distance from the eye of the player to the screen
screenDist :: Number
screenDist = 200.0

worldWidth :: Number
worldWidth = 100.0

worldDepth :: Number
worldDepth = 800.0

maxStarR :: Number
maxStarR = worldWidth

shipCircleRadius :: Number
shipCircleRadius = 40.0

angUnit :: Number
angUnit = pi/1000.0

shipDrag :: Number
shipDrag = 0.005

shipAccel :: Number
shipAccel = 3.0*angUnit

shipMaxVel :: Number
shipMaxVel = 20.0*angUnit

blasterVel :: Number
blasterVel = 100.0

framesPerSecond :: Number
framesPerSecond = 60.0

numStars :: Int
numStars = 70

generatedStars :: Int
generatedStars = 1000

minStarVel :: Number
minStarVel = 20.0

maxStarVel :: Number
maxStarVel = 100.0

starRadius :: Number
starRadius = 0.25

enemyRadius :: Number
enemyRadius = 3.0

maxBlasterBalls :: Int
maxBlasterBalls = 3

--
-- The distance a blaster ball must travel before you can fire another
--
blasterRechargeDistance :: Number
blasterRechargeDistance = 10.0

-- strictly these should be Ints but it's more convenient for them
-- to be Number`s
type Size = { w :: Number
            , h :: Number }

type Pos = { x :: Number, y :: Number }

type Pos3 = { x :: Number, y :: Number, z :: Number }

type Polar = { r :: Number
             , ang :: Number }

type Star =
  { r   :: Number
  , ang :: Number
  , vel :: Number
  }

type KeyStates = { clockwise :: KeyState
                 , anticlockwise :: KeyState
                 , fire :: KeyState}

data KeyState
  = KeyUp
  | KeyDown

data Msg
  = Fire                KeyState
  | Clockwise           KeyState
  | Anticlockwise       KeyState
  | MouseMove           (Size -> Pos)
  | Tick                Number
  | Resize              Size
  | NoOp

data SoundEvent
  = FireSound

type Ship =
  { ang      :: Number
  , angVel   :: Number
  , blasters :: List Polar -- empty means it hasn't been fired
  }

type State =
  { context2D          :: Context2D
  , canvas             :: CanvasElement
  , keys               :: KeyStates
  , ship               :: Ship
  , screenSize         :: Size
--  , sounds             :: Sounds
  , soundEvents        :: List SoundEvent
  , starCollectionIdx  :: Int
  , starCollection     :: LL.List Star
  , starField          :: LL.List Star
  , time               :: Time -- cumulative time
  , enemies            :: List Enemy
  }

-- type Sounds = {
--      context         :: AudioContext
--    , musicBuffer     :: Maybe AudioBuffer
--    , fireBuffer      :: Maybe AudioBuffer
--    }

type Time = Number

{-
Ships have a flight path that start at a particular time and then
end. Once they reach the end of their flight path they enter a
holding pattern.
-}
type Enemy =
  { enemyId :: Int, flightPos :: Time -> Pos3 }