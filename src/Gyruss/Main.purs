module Gyruss.Main where

import Gyruss.Types
import Gyruss.Sounds

import Prelude

import Audio.WebAudio.Types
import Control.Monad.Eff (Eff)
import Control.Monad
import Control.Monad.ST
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Int
import DOM
import Graphics.Canvas
import Math
import Data.DOM.Simple.Types
import Data.DOM.Simple.Window
import Data.DOM.Simple.Events

import Debug.Trace

worldWidth :: Number
worldWidth = 100.0


shipCircleRadius :: Number
shipCircleRadius = 40.0

blasterVel :: Number
blasterVel = 200.0

framesPerSecond :: Number
framesPerSecond = 30.0

--------------------------------------------------------------------------------

main :: forall s e. Eff (st :: ST s, wau :: WebAudio, canvas :: Canvas, dom :: DOM | e) Unit
main = do
  sounds <- makeSounds
  state <- defaultState sounds
  st <- newSTRef state
  resize st
  setInterval globalWindow (1000.0/framesPerSecond) $ render st
  subscribeKeyCode st KeydownEvent keydown
  subscribeTick st
  subscribeMousePos st MouseMoveEvent mouseMove
  startSounds st
  return unit

-- |Handles window resizing by stretching the canvas to fit the
-- viewport and updating our notion of how large the canvas is.
resize :: forall s e. STRef s State -> (Eff ( st :: ST s, dom :: DOM
                                           , canvas :: Canvas | e) Unit)
resize st = do
  w     <- innerWidth globalWindow
  h     <- innerHeight globalWindow
  (Just canvas) <- getCanvasElementById "canvas"
  setCanvasWidth  w canvas
  setCanvasHeight h canvas
  modifySTRef st $ (\s -> s { screenSize = s.screenSize { w = w, h = h } })
  return unit

angleFor :: { x :: Number, y :: Number } -> Number
angleFor p = atan2 p.y p.x

ship :: { x :: Number, y :: Number } -> Ship
ship p  = let pos = calcShipPos p
          in  { pos: pos, blaster: Nothing }

calcShipPos :: { x :: Number, y :: Number } -> { x :: Number, y :: Number}
calcShipPos p =
  let ang = angleFor p
  in { x: shipCircleRadius*cos ang, y: -shipCircleRadius*sin ang }

defaultState :: forall e. Sounds -> (Eff (canvas :: Canvas | e) State)
defaultState sounds = do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  return $
    { ship:       ship { x: worldWidth/2.0, y: worldWidth }
    , screenSize: { w: 0.0, h: 0.0}
    , context2D:  ctx
    , sounds: sounds
    }


-- UPDATE

update :: Msg -> State -> State
update msg g =
  let s = g.ship
  in case msg of
       NoOp -> g
       Resize sz -> g { screenSize = sz }
       MouseMove f ->
         let pos' = calcShipPos (f g.screenSize)
         in  g { ship = s { pos = pos' }}
       Fire ->
         let b = s.blaster
             ang = angleFor s.pos
             b' =
               case s.blaster of
                 Nothing -> Just (Tuple 0.0 ang)
                 _ -> b
         in g { ship = s { blaster = b' }}
       Tick delta ->
         let b = s.blaster
             ship' =  s { blaster = b'}
             b' =
               case s.blaster of
                 Just (Tuple d ang') ->
                   if d > shipCircleRadius
                     then Nothing
                     else Just (Tuple (d + blasterVel*delta) ang')
                 Nothing -> b
         in  g { ship = ship'}

--------------------------------------------------------------------------------
-- Subscriptions
subscribeKeyboardGeneric ::
  forall a s eff.
     (forall eff'. DOMEvent -> Eff (dom :: DOM | eff') a)
  -> STRef s State
  -> KeyboardEventType
  -> (a -> Msg)
  -> Eff (dom :: DOM | eff) Unit
subscribeKeyboardGeneric f st evType toMsg = do
  addKeyboardEventListener evType g globalWindow
  where
    g ev = do
      a <- f ev
      modifySTRef st $ \s -> update (toMsg a) s
      return unit

subscribeKeyCode ::
  forall s eff.
    STRef s State
  -> KeyboardEventType
  -> (Int -> Msg)
  -> Eff (dom :: DOM | eff) Unit
subscribeKeyCode = subscribeKeyboardGeneric keyCode

subscribeKey ::
  forall s eff.
    STRef s State
  -> KeyboardEventType
  -> (String -> Msg)
  -> Eff (dom :: DOM | eff) Unit
subscribeKey = subscribeKeyboardGeneric key

subscribeMousePos st evType toMsg = do
  addMouseEventListener evType g globalWindow
  where
    g ev = do
      x <- clientX (ev :: DOMEvent)
      y <- clientY ev
      modifySTRef st $ \s -> update (toMsg { x: x, y: y }) s
      return unit

subscribeTick st = do
  setInterval globalWindow (1000.0/framesPerSecond) f
  where
    f = do
      modifySTRef st $ \s -> update (Tick (1000.0/framesPerSecond)) s
      return unit

keydown :: Int -> Msg
keydown code = do
  case code of
       32 -> Fire
       _  -> NoOp

mouseMove :: { x :: Int, y :: Int} -> Msg
mouseMove pos = MouseMove (\sz -> mouseToWorld pos sz )

--------------------------------------------------------------------------------

render :: forall s e. STRef s State -> Eff (st :: ST s, canvas :: Canvas| e) Unit
render st = do
  s <- readSTRef st
  let ctx = s.context2D
  setFillStyle "#000000" ctx
  fillPath ctx $ rect ctx { x: 0.0, y: 0.0
                          , w: s.screenSize.w, h: s.screenSize.h }

  renderShip s "#ffffff"
  return unit

--
-- `toScreen` expects an effect that draws graphics
-- using a right-handed mathematical co-ordinate system. i.e. x increases
-- left to right and y increases bottom to top.
-- It converts from world co-ordinates to screen co-ordinates.
--
toScreen ::
  forall e.
     State
  -> Eff (canvas :: Canvas | e) Unit
  -> Eff (canvas :: Canvas | e) Unit
toScreen s eff = do
  let ctx = s.context2D
      side = min s.screenSize.w s.screenSize.h
  let sf = side/worldWidth
  save ctx
  translate { translateX: s.screenSize.w/2.0, translateY: s.screenSize.h/2.0 } ctx
  scale { scaleX: sf, scaleY: -sf } ctx
  eff
  restore ctx
  return unit

renderShip :: forall e. State -> String -> Eff ( canvas :: Canvas | e ) Unit
renderShip s color = toScreen s $ do
  let ctx = s.context2D
  save ctx
  translate { translateX: s.ship.pos.x, translateY: s.ship.pos.y } ctx
  rotate (atan2 s.ship.pos.y s.ship.pos.x + pi/2.0) ctx
  setLineWidth 0.2 ctx
  setStrokeStyle color ctx
  beginPath ctx
  moveTo ctx 0.0    (5.0)
  lineTo ctx (-3.5) (-5.0)
  lineTo ctx 0.0    (-4.0)
  lineTo ctx 3.5    (-5.0)
  lineTo ctx 0.0    (5.0)
  stroke ctx
  when false $ do
    moveTo ctx (-4.0) (-11.0)
    lineTo ctx 0.0    (-15.0)
    lineTo ctx 4.0    (-11.0)
    stroke ctx
    return unit

  restore ctx
  return unit

--------------------------------------------------------------------------
-- UTILITIES

mouseToWorld :: { x :: Int, y :: Int } -> Size -> Pos
mouseToWorld pos sz =
  let x' = toNumber pos.x
      y' = toNumber pos.y
      side = min sz.w sz.h
      sf = worldWidth/side
   in { x: (sf*(x' - sz.w/2.0)), y: (sf*(y' - sz.h/2.0)) }

