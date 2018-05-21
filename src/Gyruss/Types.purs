module Gyruss.Types where


-- import Audio.WebAudio.Types
import Control.Monad.Eff
import Control.Monad.Eff.Random
import Data.Maybe (Maybe)
import Data.List
import Data.List.Lazy as LL
import Data.Tuple
import Graphics.Canvas


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
  { ang     :: Number
  , angVel  :: Number
  , blaster :: Maybe Polar -- Nothing means it hasn't been fired
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
  { flightPos :: Time -> Pos3 }


