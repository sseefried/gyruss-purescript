module Gyruss.Main where

import Gyruss.Types
import Gyruss.Render
import Gyruss.Util

import Control.Monad
import Control.Monad.Eff.Random
import Control.Monad.Eff.Timer
import Control.Monad.Except.Trans
import Control.Monad.ST
import DOM
import DOM.Event.EventTarget
import DOM.Event.KeyboardEvent hiding (KeyLocation(..))
import DOM.Event.MouseEvent
import DOM.Event.Types
import DOM.HTML
import DOM.HTML.Types
import DOM.HTML.Window hiding (moveTo)
import Data.Either
import Data.Foldable
import Data.Foreign
import Data.Identity
import Data.Int
import Data.List
import Data.List.Types
import Data.Maybe
import Data.Traversable
import Math hiding (min,max)
import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except (runExcept)
import Data.Int as Int
import Data.List as L
import Data.List.Lazy as LL
import Data.List.Lazy.Types as LL
import Math as Math
import Partial.Unsafe (unsafePartial)

import Debug.Trace



{-

Principles of input handling:

* All updating occurs on ticks
* key events change keystate from up to down, or vice versa

-}

--------------------------------------------------------------------------------

main :: forall s e. Eff (random :: RANDOM, st :: ST s{-, wau :: WebAudio-}
                        , console :: CONSOLE, canvas :: CANVAS, dom :: DOM, timer :: TIMER | e) Unit
main = do
--  sounds <- makeSounds
--  state <- defaultState sounds
  state <- mkDefaultState
  win <- window
  let target = windowToEventTarget win
  st <- newSTRef state
  resize st
  void $ setInterval (Int.floor (1000.0/framesPerSecond)) $ render st
  addEventListener (EventType "keydown")
    -- TODO: refactor
    (eventListener $ eventListenerFor st keyCode keydown) false target
  addEventListener (EventType "keyup")
    (eventListener $ eventListenerFor st keyCode keyup) false target
  void $ subscribeTick st
  subscribeMousePos st (EventType "mousemove") mouseMove
  addEventListener (EventType "resize")
    (eventListener $ eventListenerFor st (const getScreenSize) Resize) false target
--  startSounds st
  pure unit


getScreenSize :: forall eff.  Eff (dom :: DOM | eff) Size
getScreenSize = do
  globalWindow <- window
  w <- innerWidth globalWindow
  h <- innerHeight globalWindow
  pure $ { w: toNumber w, h: toNumber h }

-- | Handles window resizing by stretching the canvas to fit the
-- viewport and updating our notion of how large the canvas is.
resize :: forall s e. STRef s State -> (Eff ( st :: ST s, dom :: DOM
                                           , canvas :: CANVAS | e) Unit)
resize st = unsafePartial $ do
  globalWindow <- window
  w     <- innerWidth globalWindow
  h     <- innerHeight globalWindow
  Just canvas <- getCanvasElementById "canvas"
  let w' = toNumber w*0.99
      h' = toNumber h*0.99
  void $ setCanvasWidth  w' canvas
  void $ setCanvasHeight h' canvas
  void $ modifySTRef st $ (\s -> s { screenSize = s.screenSize { w = w', h = h' } })
  pure unit



angleFor :: { x :: Number, y :: Number } -> Number
angleFor p = atan2 p.y p.x

newShip :: Number -> Ship
newShip ang  = { ang: ang, angVel: 0.0, blaster: Nothing }



mkDefaultState :: forall e. {-Sounds ->-} Eff (random :: RANDOM, canvas :: CANVAS | e) State
mkDefaultState {-sounds-} = unsafePartial $ do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  stars <- randomStars generatedStars
  pure $
    { ship:              newShip (-pi/2.0)
    , keys:              { clockwise: KeyUp
                         , anticlockwise: KeyUp
                         , fire: KeyUp }
    , screenSize:        { w: 0.0, h: 0.0}
    , canvas:            canvas
    , context2D:         ctx
--    , sounds:            sounds
    , soundEvents:       Nil
    , starCollectionIdx: 0
    , starCollection:    stars
    , starField:         LL.take numStars stars
    , time:              0.0
    , enemies:           map toEnemy (0.4 : 0.8 : 1.2: 1.6 : Nil)
    }

  where
    toEnemy delta = { enemyId: 1, flightPos: flightPosFun delta }
    maxDist = -worldDepth
    flightPosFun delta t = { x: distU * cosU (angU delta)
                           , y: distU * sinU (angU delta)
                           , z: maxDist/2.0 + maxDist/2.0 * sinU (0.1*t)
                           }
      where
         -- Enemy just moves in a pattern dependent on time
         angU delta = (fmod (t+delta) 6.0)/6.0
         distU = shipCircleRadius * 0.3


sinU :: Number -> Number
sinU x = sin (2.0*pi*x)

cosU :: Number -> Number
cosU x = cos (2.0*pi*x)

fmod :: Number -> Number -> Number
fmod x y = x - Math.floor (x/y)*y

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
         -- update movement
         let s1 =
               let sh = s.ship
               in case s.keys of
                    { clockwise: KeyDown } ->
                      let angVel' = max (sh.angVel - shipAccel) (-shipMaxVel)
                      in  modShip s $ \sh -> sh { ang  = sh.ang + angVel'
                                                , angVel = angVel' }
                    { anticlockwise: KeyDown } ->
                      let angVel' = min (sh.angVel + shipAccel) (shipMaxVel)
                      in  modShip s $ \sh -> sh { ang  = sh.ang + angVel'
                                                , angVel = angVel' }

                    _ ->
                      let ang' = sh.ang + sh.angVel
                          absV  = abs sh.angVel - shipDrag
                          angVel' = if absV > 0.0 then absV * sign sh.angVel else 0.0
                      in modShip s $ \sh -> sh { ang = ang', angVel = angVel' }
             -- update the blaster
             (s2 :: State) =
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
             -- check for collisions with enemies
             (s3 :: State) =
               let sh = s2.ship
               in case sh.blaster of
                    Just pp ->
                      let noCollision e =
                            let shipP   = polarToPos (actualBlasterPos pp)
                                enemyP3 = e.flightPos s2.time
                            in not $ posIsectPos3
                                       shipP (blasterRadius pp)
                                       enemyP3 (scaleFactor enemyP3* enemyRadius)
                          (enemies':: List Enemy) = filter noCollision s2.enemies
                      in s2 { enemies = enemies' }
                    Nothing -> s2
             -- TODO: This is quite inefficient
             (s4 :: State) =
               let sh = s3.ship
               in if L.length s2.enemies > L.length s3.enemies
                    then s3 { ship = sh { blaster = Nothing }}
                    else s3
         in  updateTime delta (updateStars delta s4)
      -- catch-all
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
    updateTime delta s = s { time = s.time + delta }
    updateStars delta s = rec.s { starField = rec.newStars }
      where
        rec = foldl step { s: s, newStars: LL.nil} s.starField
        step :: { s :: State, newStars :: LL.List Star } -> Star
             -> { s :: State, newStars :: LL.List Star }
        step rec star =
          if star.r > maxStarR
            then { s:        rec.s { starCollectionIdx = (rec.s.starCollectionIdx + 1) `mod` generatedStars }
                 , newStars: LL.snoc rec.newStars (unsafePartial (fromJust (rec.s.starCollection LL.!! rec.s.starCollectionIdx))) }
            else { s:        rec.s
                 , newStars: LL.snoc rec.newStars (star { r = star.r + star.r/maxStarR*delta*star.vel }) }

sign :: Number -> Number
sign n = if n < 0.0 then -1.0 else 1.0

--------------------------------------------------------------------------------
-- Subscriptions

eventListenerFor
  :: forall a s eff.
      STRef s State
  -> (forall eff'. Event -> Eff (console :: CONSOLE, dom :: DOM | eff') a)
  -> (a -> Msg)
  -> (Event -> Eff (console :: CONSOLE, st :: ST s, dom :: DOM | eff) Unit)
eventListenerFor st fromEvent toMsg = \ev -> do
  a <- fromEvent ev
  void $ modifySTRef st $ \s -> update (toMsg a) s
  pure unit

subscribeMousePos
  :: forall s eff. STRef s State -> EventType -> ({x :: Int, y :: Int} -> Msg)
  -> Eff (st :: ST s, dom :: DOM | eff) Unit
subscribeMousePos st evType toMsg = do
  win <- window
  addEventListener evType (eventListener g) false (windowToEventTarget win)
  where
--    g :: forall s eff. Event -> Eff (st :: ST s, dom :: DOM | eff) Unit
    g ev = do
      case runExcept (eventToMouseEvent ev) of
        Right mev -> do
          let x = clientX mev
              y = clientY mev
          void $ modifySTRef st $ \s -> update (toMsg { x: x, y: y }) s
          pure unit
        Left _ -> pure unit

subscribeTick :: forall s eff. STRef s State -> Eff (st :: ST s, timer :: TIMER | eff) Unit
subscribeTick st = void $ setInterval (Int.floor (1000.0/framesPerSecond)) f
  where
    f = void $ modifySTRef st $ \s -> update (Tick (1.0/framesPerSecond)) s


keydown :: String -> Msg
keydown code =
  case code of
     "Space" -> Fire KeyDown
     "ArrowLeft" -> Clockwise KeyDown
     "ArrowRight" -> Anticlockwise KeyDown
     _  -> NoOp

keyup :: String -> Msg
keyup code =
  case code of
     "Space" -> Fire KeyUp
     "ArrowLeft" -> Clockwise KeyUp
     "ArrowRight" -> Anticlockwise KeyUp
     _  -> NoOp

mouseMove :: { x :: Int, y :: Int} -> Msg
mouseMove pos = MouseMove (\sz -> mouseToWorld pos sz )

--------------------------------------------------------------------------------

keyCode :: forall eff. Event -> Eff (dom :: DOM, console :: CONSOLE | eff) String
keyCode ev =
  case runExcept (eventToKeyboardEvent ev) of
    Right kev -> pure $ code kev
    Left _    -> pure "no keycode"

--------------------------------------------------------------------------
-- UTILITIES

mouseToWorld :: { x :: Int, y :: Int } -> Size -> Pos
mouseToWorld pos sz =
  let x' = toNumber pos.x
      y' = toNumber pos.y
      side = min sz.w sz.h
      sf = worldWidth/side
   in { x: (sf*(x' - sz.w/2.0)), y: (sf*(y' - sz.h/2.0)) }

randomStars ::  Int -> forall e. Eff (random :: RANDOM | e) (LL.List Star)
randomStars n = LL.replicateM n newRandomStar

newRandomStar :: forall e. Eff (random :: RANDOM | e) Star
newRandomStar = do
  ang <- randomRange 0.0 (2.0 * pi)
  r   <- randomRange 0.0 (worldWidth/2.0)
  vel <- randomRange minStarVel maxStarVel
  pure { r: r, ang: ang, vel: vel }