module Gyruss.Types where


import Audio.WebAudio.Types
import Data.Maybe (Maybe)
import Data.Tuple
import Graphics.Canvas


-- strictly these should be Ints but it's more convenient for them
-- to be Number`s
type Size = { w :: Number
            , h :: Number
            }

type Pos = { x :: Number, y :: Number }

data Msg
  = Fire
  | MouseMove (Size -> Pos)
  | Tick Number
  | NoOp
  | Resize Size

type Ship =
  { pos     :: { x :: Number, y :: Number }
  , blaster :: Maybe (Tuple Number Number) -- Nothing means it hasn't been fired
  }

type State =
  { ship       :: Ship
  , screenSize :: Size
  , context2D  :: Context2D
  , sounds     :: Sounds
  }

type Sounds = {
      context         :: AudioContext
    , musicBuffer     :: Maybe AudioBuffer
    , fireBuffer      :: Maybe AudioBuffer
    }
