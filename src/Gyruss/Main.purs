module Gyruss.Main where

import Gyruss.Types
import Gyruss.Sounds

import Prelude

import Audio.WebAudio.Types
import Control.Monad.Eff (Eff)
import Control.Monad
import Control.Monad.ST
import Data.List
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Traversable
import Data.Int
import DOM
import Graphics.Canvas
import Math
import Data.DOM.Simple.Types
import Data.DOM.Simple.Window
import Data.DOM.Simple.Events

import Debug.Trace

{-

Principles of input handling:

* All updating occurs on ticks
* key events change keystate from up to down, or vice versa

-}


worldWidth :: Number
worldWidth = 100.0


shipCircleRadius :: Number
shipCircleRadius = 40.0

angUnit :: Number
angUnit = pi/1000.0

shipDrag :: Number
shipDrag = pi/400.0

shipAccel :: Number
shipAccel = 5.0*angUnit

shipMaxVel :: Number
shipMaxVel = 30.0*angUnit

blasterVel :: Number
blasterVel = 200.0

framesPerSecond :: Number
framesPerSecond = 60.0

--------------------------------------------------------------------------------

main :: forall s e. Eff (st :: ST s, wau :: WebAudio, canvas :: Canvas, dom :: DOM | e) Unit
main = do
  sounds <- makeSounds
  state <- defaultState sounds
  st <- newSTRef state
  resize st
  setInterval globalWindow (1000.0/framesPerSecond) $ render st
  addKeyboardEventListener KeydownEvent
    (eventListenerFor st keyCode keydown) globalWindow
  addKeyboardEventListener KeyupEvent
    (eventListenerFor st keyCode keyup) globalWindow
  subscribeTick st
  subscribeMousePos st MouseMoveEvent mouseMove
  addUIEventListener ResizeEvent
    (eventListenerFor st (const getScreenSize) Resize) globalWindow
  startSounds st
  return unit

getScreenSize :: forall eff.  Eff (dom :: DOM | eff) Size
getScreenSize = do
  w <- innerWidth globalWindow
  h <- innerHeight globalWindow
  return $ { w: w, h: h }

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

newShip :: Number -> Ship
newShip ang  = { ang: ang, angVel: 0.0, blaster: Nothing }

shipPos :: Ship -> Pos
shipPos ship = { x:  shipCircleRadius*cos ship.ang
               , y: -shipCircleRadius*sin ship.ang }


defaultState :: forall e. Sounds -> (Eff (canvas :: Canvas | e) State)
defaultState sounds = do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  return $
    { ship:       newShip (pi/2.0)
    , keys: { clockwise: KeyUp, anticlockwise: KeyUp, fire: KeyUp }
    , screenSize: { w: 0.0, h: 0.0}
    , canvas:     canvas
    , context2D:  ctx
    , sounds:     sounds
    , soundEvents: Nil
    }

-- UPDATE

update :: Msg -> State -> State
update msg s =
  case msg of
      NoOp -> s
      Resize sz -> s { screenSize = sz }
      Clockwise ks      -> modKeys $ \k -> k { clockwise = ks }
      Anticlockwise  ks -> modKeys $ \k -> k { anticlockwise = ks}
      Fire ks -> modKeys $ \k -> k { fire = ks }
      Tick delta ->
         let s1 =
               let sh = s.ship
               in case s.keys of
                    { clockwise: KeyDown } ->
                      let angVel' = min (sh.angVel + shipAccel) shipMaxVel
                      in  modShip s $ \sh -> sh { ang    = sh.ang + angVel'
                                              , angVel = angVel' }
                    { anticlockwise: KeyDown } ->
                      let angVel' = max (sh.angVel - shipAccel) (-shipMaxVel)
                      in  modShip s $ \sh -> sh { ang    = sh.ang + angVel'
                                              , angVel = angVel' }

                    _ ->
                      let ang' = sh.ang + sh.angVel
                          absV  = abs sh.angVel - shipDrag
                          angVel' = if absV > 0.0 then absV * sign sh.angVel else 0.0
                      in modShip s $ \sh -> sh { ang = ang', angVel = angVel' }
             s2 =
               let sh = s1.ship
               in case s1.keys.fire of
                    KeyUp ->
                      modShip s1 $ \sh -> sh { blaster = blasterContinue sh delta }
                    KeyDown ->
                      let b = sh.blaster
                          rec =
                            case b of
                              Nothing -> { b': Just { r: 0.0, ang: sh.ang }
                                         , snds: Cons FireSound s.soundEvents }
                              _ ->       { b': blasterContinue sh delta
                                         , snds: s.soundEvents }
                      in s1 { ship = sh { blaster = rec.b' }
                             , soundEvents = rec.snds }
         in  s2
        -- catchall
      _ -> s
  where
    modShip s f = s { ship = f s.ship }
    modShipVel s f =
      modShip s $ \sh ->
        let angVel' = clamp (-shipMaxVel) (f sh.angVel) shipMaxVel
        in  sh { angVel = angVel' }
    modKeys f = s { keys = f s.keys }
    blasterContinue sh delta =
      case sh.blaster of
        Just pp ->
          if pp.r > shipCircleRadius
            then Nothing
            else Just { r:   pp.r + blasterVel*delta, ang: pp.ang }
        Nothing -> sh.blaster


clamp :: Number -> Number -> Number -> Number
clamp a x b = min b (max a x)

sign :: Number -> Number
sign n = if n < 0.0 then -1.0 else 1.0

--------------------------------------------------------------------------------
-- Subscriptions

eventListenerFor
  :: forall a s eff.
      STRef s State
  -> (forall eff'. DOMEvent -> Eff (dom :: DOM | eff') a)
  -> (a -> Msg)
  -> (DOMEvent -> Eff (st :: ST s, dom :: DOM | eff) Unit)
eventListenerFor st fromEvent toMsg = \ev -> do
  a <- fromEvent ev
  modifySTRef st $ \s -> update (toMsg a) s
  return unit

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
      modifySTRef st $ \s -> update (Tick (1.0/framesPerSecond)) s
      return unit

keydown :: Int -> Msg
keydown code =
  case code of
     32 -> Fire KeyDown
     37 -> Clockwise KeyDown
     39 -> Anticlockwise KeyDown
     _  -> NoOp

keyup :: Int -> Msg
keyup code =
  case code of
     32 -> Fire KeyUp
     37 -> Clockwise KeyUp
     39 -> Anticlockwise KeyUp
     _  -> NoOp

mouseMove :: { x :: Int, y :: Int} -> Msg
mouseMove pos = MouseMove (\sz -> mouseToWorld pos sz )

--------------------------------------------------------------------------------

render :: forall s e. STRef s State
       -> Eff (st :: ST s, wau :: WebAudio, canvas :: Canvas| e) Unit
render st = do
  s <- readSTRef st
  let sz = s.screenSize
  -- FIXME: setting the canvas size every time is just dumb!
  setCanvasWidth  sz.w s.canvas
  setCanvasHeight sz.h s.canvas
  let ctx = s.context2D
  setFillStyle "#000000" ctx
  fillPath ctx $ rect ctx { x: 0.0, y: 0.0
                          , w: s.screenSize.w, h: s.screenSize.h }
  renderShip s "#ffffff"
  traverse (playSoundEvent s.sounds) s.soundEvents
  modifySTRef st $ \s -> s { soundEvents = Nil }
  return unit

playSoundEvent :: forall e. Sounds -> SoundEvent -> Eff (wau :: WebAudio | e) Unit
playSoundEvent sounds ev =
  case ev of
    FireSound -> playBufferedSound sounds sounds.fireBuffer


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
  let p = shipPos s.ship
  translate { translateX: p.x, translateY: p.y } ctx
  rotate (pi/2.0 - s.ship.ang) ctx
  setLineWidth 0.2 ctx
  setStrokeStyle color ctx
  beginPath ctx
  moveTo ctx 0.0    (5.0)
  lineTo ctx (-3.5) (-5.0)
  lineTo ctx 0.0    (-4.0)
  lineTo ctx 3.5    (-5.0)
  lineTo ctx 0.0    (5.0)
  stroke ctx
  restore ctx
  case s.ship.blaster of
    Just pp -> do
      save ctx
      setFillStyle color ctx

      translate { translateX: shipCircleRadius*cos pp.ang
                , translateY: -shipCircleRadius*sin pp.ang } ctx
      beginPath ctx
      arc ctx { x: -pp.r * cos pp.ang
              , y: pp.r * sin pp.ang
              , r: 0.75
              , start: 0.0
              , end: 2.0*pi }
      stroke ctx
      fill ctx
      restore ctx
      return unit
    Nothing -> return unit

  when false $ do
    moveTo ctx (-4.0) (-11.0)
    lineTo ctx 0.0    (-15.0)
    lineTo ctx 4.0    (-11.0)
    stroke ctx
    return unit


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

