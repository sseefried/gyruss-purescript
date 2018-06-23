module Gyruss.Types where

import Prelude ((*), (/))

-- import Audio.WebAudio.Types
import Data.List (List)
import Data.List.Lazy as LL
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CanvasElement, Context2D)
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
shipDrag = 0.01

shipAccel :: Number
shipAccel = 2.0*angUnit

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
minStarVel = 100.0

maxStarVel :: Number
maxStarVel = 200.0

starRadius :: Number
starRadius = 0.25

enemyRadius :: Number
enemyRadius = 2.0

maxBlasterBalls :: Int
maxBlasterBalls = 3

bombRadius :: Number
bombRadius = 0.4

bombSpeed :: Number
bombSpeed = 2.0

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
  , enemyWaves         :: Map EnemyWaveId EnemyWave
  , bombs              :: List Bomb
  , score              :: Int
  }

data EnemySort = Normal

type EnemyWaveId = Int

--
-- An EnemyWave
--
type EnemyWave =
  { arriveTime    :: Time
  , enemies       :: List Enemy
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

--
-- * index points the path segment (pathSegements[index]) that is
--   currently being rendered (we call this "current path segment")
-- * startedAt is the world time at which the current path segment started
--   rendering. If `Nothing` then the enemy does not render
-- * delta is a short delay after which the enemy appears. this
--   will affect its startedAt value
--
-- if index = pathSegments.length then the enemy is considered
--
type Enemy = { startedAt     :: Maybe Time
             , sort          :: EnemySort
             , delta         :: Number -- a delay time after which this enemy appears
             , index         :: Int
             , pathSegments  :: Array PathSegment
             , releaseBombAt :: Maybe Time
             }

--
-- `duration` is a period of time over which the function should be sampled.
-- Call the parameter that is used to sample the function, t.
-- t will be linearly scaled to another value t' before being pass to `func`.
-- `funcStart` and `funcFinish` describe the start and finish values of
-- t' for the function
--
-- t is scaled to t' according to the following conditions:
--   * if t = 0        then t' = funcStart
--   * if t = duration then t' = funcFinish
--
type PathSegment =
  { duration   :: Time
  , func       :: Time -> Pos3
  , funcStart  :: Time
  , funcFinish :: Time
  }

type Bomb =
  { pos :: Pos
  , dir :: { x :: Number, y :: Number }
  -- ^ unit vector signifying direction of bomb travel
  }
