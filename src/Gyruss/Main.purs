module Gyruss.Main where

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
import Debug.Trace
import Graphics.Canvas
import Gyruss.Types
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

foreign import windowToEventTarget :: Window -> EventTarget


{-

Principles of input handling:

* All updating occurs on ticks
* key events change keystate from up to down, or vice versa

-}


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
blasterVel = 200.0

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

shipPos :: Ship -> Pos
shipPos ship = { x:  shipCircleRadius*cos ship.ang
               , y:  shipCircleRadius*sin ship.ang }


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

render :: forall s e. STRef s State
       -> Eff (st :: ST s, random :: RANDOM
              {-, wau :: WebAudio-}, canvas :: CANVAS| e) Unit
render st = do
  s <- readSTRef st
  let sz = s.screenSize
  -- FIXME: setting the canvas size every time is just dumb!
  void $ setCanvasWidth  sz.w s.canvas
  void $ setCanvasHeight sz.h s.canvas
  let ctx = s.context2D
  void $ setFillStyle "#000000" ctx
  void $ fillPath ctx $ rect ctx { x: 0.0, y: 0.0
                          , w: s.screenSize.w, h: s.screenSize.h }
  toScreen s $ do
    renderStars s
    renderShip s "#ffffff"
    traverse_ (renderEnemy s.context2D s.time) s.enemies

--  traverse (playSoundEvent s.sounds) s.soundEvents
  void $ modifySTRef st $ \s -> s { soundEvents = Nil }
  pure unit

-- playSoundEvent :: forall e. Sounds -> SoundEvent -> Eff (wau :: WebAudio | e) Unit
-- playSoundEvent sounds ev =
--   case ev of
--     FireSound -> playBufferedSound sounds sounds.fireBuffer


renderStars :: forall e.
               State
            -> Eff (canvas :: CANVAS | e) Unit
renderStars s = do
  let ctx = s.context2D
      renderStar star = do
        void $ save ctx
        let v = Int.floor (star.r/maxStarR*255.0)
        void $ setFillStyle (colorStr v v v) ctx
        void $ fillCircle ctx { x: star.r * cos star.ang
                       , y: star.r * sin star.ang
                       , r: starRadius }
        void $ restore ctx
        pure unit

  void $ traverse renderStar s.starField
  pure unit

fillCircle :: forall e. Context2D
           -> { x :: Number, y :: Number, r :: Number }
           -> Eff (canvas :: CANVAS | e) Unit
fillCircle ctx o = fillPath ctx $ circlePath ctx o


circlePath :: forall e. Context2D ->  { x :: Number, y :: Number, r :: Number }
           -> Eff (canvas :: CANVAS | e) Unit
circlePath ctx o = do
  void $ arc ctx
          { x: o.x
          , y: o.y
          , r: o.r
          , start: 0.0
          , end: 2.0*pi }
  pure unit

colorStr :: Int -> Int -> Int -> String
colorStr r g b = "rgb("<>show r<>","<>show g<>","<>show b<>")"

--
-- `toScreen` expects an effect that draws graphics
-- using a right-handed mathematical co-ordinate system. i.e. x increases
-- left to right and y increases bottom to top.
-- It converts from world co-ordinates to screen co-ordinates.
--
toScreen ::
  forall e.
     State
  -> Eff (canvas :: CANVAS | e) Unit
  -> Eff (canvas :: CANVAS | e) Unit
toScreen s eff = do
  let ctx = s.context2D
      side = min s.screenSize.w s.screenSize.h
  let sf = side/worldWidth
  _ <- save ctx
  _ <- translate { translateX: s.screenSize.w/2.0, translateY: s.screenSize.h/2.0 } ctx
  _ <- scale { scaleX: sf, scaleY: -sf } ctx
  eff
  _ <- restore ctx
  pure unit

renderShip :: forall e. State -> String -> Eff ( canvas :: CANVAS | e ) Unit
renderShip s color = do
  let ctx = s.context2D
  void $ save ctx
  let p = shipPos s.ship
  void $ translate { translateX: p.x, translateY: p.y } ctx
  void $ rotate (pi/2.0 + s.ship.ang) ctx
  void $ setLineWidth 0.2 ctx
  void $ setStrokeStyle color ctx
  void $ beginPath ctx
  void $ moveTo ctx 0.0    (2.0)
  void $ lineTo ctx (-4.5) (-3.0)
  void $ lineTo ctx 0.0    (-2.0)
  void $ lineTo ctx 4.5    (-3.0)
  void $ lineTo ctx 0.0    (2.0)
  void $ stroke ctx
  void $ closePath ctx
  void $ restore ctx
  case s.ship.blaster of
    Just pp -> do
      void $ save ctx
      void $ setFillStyle (colorStr 255 255 255) ctx
      void $ translate { translateX: shipCircleRadius*cos pp.ang
                       , translateY: shipCircleRadius*sin pp.ang } ctx
      void $ beginPath ctx
      void $ circlePath ctx
              { x: -pp.r * cos pp.ang
              , y: -pp.r * sin pp.ang
              , r: blasterRadius pp
              }
      void $ stroke ctx
      void $ fill ctx
      void $ restore ctx
      pure unit
    Nothing -> pure unit

  when false $ do
    void $ moveTo ctx (-4.0) (-11.0)
    void $ lineTo ctx 0.0    (-15.0)
    void $ lineTo ctx 4.0    (-11.0)
    void $ stroke ctx
    pure unit
  pure unit

polarToPos :: Polar -> Pos
polarToPos pp = { x: pp.r * cos pp.ang, y: pp.r * sin pp.ang }

actualBlasterPos :: Polar -> Polar
actualBlasterPos pp = { r: shipCircleRadius - pp.r , ang: pp.ang }

--
-- Given the position of the blaster (relative to ship) returns the
-- radius of the blaster ball
--
blasterRadius :: Polar -> Number
blasterRadius pp = 0.5 + 0.5*(1.0 - pp.r/shipCircleRadius)

--
-- Checks whether Pos intersects with Pos3 within a certain radius
--
posIsectPos3 :: Pos -> Number -> Pos3 -> Number -> Boolean
posIsectPos3 p r p3 r' =
  let dx = p.x - p3.x
      dy = p.y - p3.y
      d  = r + r'
  in dx*dx + dy*dy < d*d

renderEnemy :: forall e. Context2D -> Time -> Enemy
            -> Eff ( canvas :: CANVAS | e ) Unit
renderEnemy ctx t en = do
  void $ save ctx
  let p = en.flightPos t
  void $ setLineWidth 0.2 ctx
  void $ setStrokeStyle "rgb(255,255,255)" ctx
  void $ setLineWidth 0.2 ctx
  void $ beginPath ctx
  let f = scaleFactor p
  void $ circlePath ctx
          { x: p.x
          , y: p.y
          , r: f*enemyRadius
          }
  void $ stroke ctx
  void $ restore ctx
  pure unit

--
-- How much to scale the x,y co-ordinates of an object that is a certain
-- distance away in the z axis.
--
scaleFactor :: Pos3 -> Number
scaleFactor p3 = screenDist / (screenDist - p3.z)


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