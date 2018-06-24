module Gyruss.Main where

import Math hiding (min,max)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM, randomRange)
import Control.Monad.Eff.Timer (TIMER, setInterval)
import Control.Monad.Except (runExcept)
import Control.Monad.ST (ST, STRef, modifySTRef, newSTRef, readSTRef)
import DOM (DOM)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.Event.KeyboardEvent (code, eventToKeyboardEvent)
import DOM.Event.MouseEvent (clientX, clientY, eventToMouseEvent)
import DOM.Event.Types (Event, EventType(..))
import DOM.HTML (window)
import DOM.HTML.Types (windowToEventTarget)
import DOM.HTML.Window (innerHeight, innerWidth)
import Data.Array (length)
import Data.Either (Either(..))
import Data.Foldable (any)
import Data.Int (toNumber)
import Data.Int as Int
import Data.List (catMaybes, zip, unzip, concat)
import Data.List as L
import Data.List.Lazy (replicateM, snoc, take, (!!)) as LL
import Data.List.Lazy.Types (List, nil) as LL
import Data.List.Types (List(..), (:))
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (foldl, sequence, traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Gyruss.Render (CANVAS, getCanvasElementById, getContext2D, renderLevel
                     , renderGameOver, setCanvasHeight, setCanvasWidth)
import Gyruss.Types (Enemy, EnemySort(..), EnemyWave, KeyState(..), Msg(..)
                    , Polar, Vec2, Ship, Size, SoundEvent(..), Star, State
                    , Time, Bomb, FSMState(..), blasterRechargeDistance
                    , blasterVel, enemyRadius, framesPerSecond, generatedStars
                    , maxBlasterBalls, maxStarR, maxStarVel, minStarVel
                    , numStars, shipAccel, shipCircleRadius, shipDrag
                    , shipMaxVel, shipRadius, bombRadius, worldDepth
                    , worldWidth, bombVel)
import Gyruss.Util (actualBlasterPos, blasterRadius, polarToPos, pos2IsectPos3
                   , pos2IsectPos2, scaleFactor, enemyPos, (!!!), shipPos
                   , enemyPoints, dirBetweenPoints, outOfBounds)
import Math (floor) as Math
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, clamp, const, discard, map, max, min, mod, negate
               , pure, unit, void, id, ($), (&&), (*), (+), (-), (/), (<), (>)
               , (>=), (==), not, (<$>), (<>), (<=))

{-

Principles of input handling:

* All updating occurs on ticks
* key events change keystate from up to down, or vice versa

-}

--------------------------------------------------------------------------------

main :: forall s e. Eff (random :: RANDOM, st :: ST s{-, wau :: WebAudio-}
                        , console :: CONSOLE, canvas :: CANVAS, dom :: DOM
                        , timer :: TIMER | e) Unit
main = do
--  sounds <- makeSounds
--  state <- defaultState sounds
  state <- mkDefaultState
  win <- window
  let target = windowToEventTarget win
  st <- newSTRef state
  resize st

  setupMainLoop st

  addEventListener (EventType "keydown")
    -- TODO: refactor
    (eventListener $ eventListenerFor st keyCode keydown) false target
  addEventListener (EventType "keyup")
    (eventListener $ eventListenerFor st keyCode keyup) false target

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
newShip ang = { ang: ang, angVel: 0.0, blasters: Nil }

mkDefaultState
  :: forall e. {-Sounds ->-}
     Eff (random :: RANDOM, canvas :: CANVAS | e) State
mkDefaultState {-sounds-} = unsafePartial $ do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  stars <- randomStars generatedStars
  waves <- sequence $   mkWave 1.0  0.0
                      : mkWave 8.5  ( pi/4.0)
                      : mkWave 16.0 (-pi/4.0)
                      : mkWave 24.5 pi
                      : Nil
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
    , enemyWaves:        waves
    , bombs:             Nil
    , score:             0
    , fsmState:          Level
    }

  where
    mkWave arriveTime angle = do
      enemies <- traverse (toEnemy angle) (zip lrs deltas)
      pure $
        { arriveTime: arriveTime
        , enemies: enemies
        }

    mkSeq _ _ 0 = Nil
    mkSeq start inc n = start : mkSeq (start+inc) inc (n-1)

    numEnemies = 10

    deltas :: List Number
    deltas = mkSeq 0.0 0.1 numEnemies

    lrs = go numEnemies true
      where
        go 0 _ = Nil
        go n b = b : go (n-1) (not b)

    toEnemy :: forall e'. Number -> Tuple Boolean Number
            -> Eff (random :: RANDOM | e') Enemy
    toEnemy angle (Tuple onLeft delta) = do
      let chanceOfRelease = 0.2
      mbReleaseBombAt <- randomBombReleaseTime 2.0 3.5 chanceOfRelease
      pure $
        { startedAt: Nothing
        , delta: delta
        , sort: Normal
        , index: 0
        , pathSegments:
            [
              { duration:   1.5
              , func:       rotateXY angle $ fun1 onLeft
              , funcStart:  0.0
              , funcFinish: pi/2.0
              }
            , { duration:   0.5
              , func:       rotateXY angle $ fun2
              , funcStart:  0.0
              , funcFinish: 0.5
              }
            , { duration:   1.5
              , func:       rotateXY angle $ fun3 onLeft
              , funcStart:  0.0
              , funcFinish: pi/2.0
              }
            ]
        , mbReleaseBombAt: mbReleaseBombAt
        }

    maxDist = -worldDepth

    smallRad = 0.7*shipCircleRadius

    scale k f t = { x: k*r.x, y: k*r.y, z: k*r.z }
      where r = f t

    rotateXY a f t = { x: r.x * cos a - r.y * sin a
                     , y: r.x * sin a + r.y * cos a
                     , z: r.z
                     }
      where r = f t

    -- Path on left or right depe
    fun1 onLeft = scale smallRad $ \t ->
                    let t' = (pi/2.0 - t)
                    in  { x: k * 0.375*sin(2.0*t')
                        , y: 0.5*sin t' + 0.125 * sin(2.0*t') - 0.5
                        , z: -8.0 * (t'*2.0/pi)
                        }
      where k = if onLeft then 1.0 else -1.0
    fun2 = scale smallRad $ \t ->
             { x: 0.0
             , y: -0.5 - t
             , z: 0.0
             }
    fun3 onLeft =  scale smallRad $ \t ->
                     { x: k * 0.375*sin(2.0*t)
                     , y: sin t + 0.25 * sin(2.0*t) - 1.0
                     , z:  - 8.0 * (t*2.0/pi)
                     }
      where k = if onLeft then 1.0 else -1.0

    xPos t = smallRad * cos t
    yPos t = smallRad * sin t


--
-- Returns either
--   a) random bomb release time between `start` and `finish`
--   b) Nothing
--
randomBombReleaseTime :: forall e. Number -> Number -> Number
                      -> Eff (random :: RANDOM | e) (Maybe Time)
randomBombReleaseTime start finish chanceOfRelease = do
  releaseVal <- randomRange 0.0 1.0
  if releaseVal <= chanceOfRelease
    then Just <$> randomRange start finish
    else pure Nothing

sinU :: Number -> Number
sinU x = sin (2.0*pi*x)

cosU :: Number -> Number
cosU x = cos (2.0*pi*x)

fmod :: Number -> Number -> Number
fmod x y = x - Math.floor (x/y)*y

-- UPDATE
updateLevel :: Msg -> State -> State
updateLevel msg s =
  case msg of
      NoOp -> s
      Resize sz -> s { screenSize = sz }
      Clockwise ks      -> modKeys s $ \k -> k { clockwise = ks }
      Anticlockwise  ks -> modKeys s $ \k -> k { anticlockwise = ks}
      Fire ks -> modKeys s $ \k -> k { fire = ks }
      Tick delta ->
        -- update movement
        let s1 =
              let sh = s.ship
              in  case s.keys of
                    { clockwise: KeyDown } ->
                      let angVel' = max (sh.angVel - shipAccel) (-shipMaxVel)
                      in  modShip s $ \sh' -> sh' { ang  = sh'.ang + angVel'
                                                  , angVel = angVel' }
                    { anticlockwise: KeyDown } ->
                      let angVel' = min (sh.angVel + shipAccel) (shipMaxVel)
                      in  modShip s $ \sh' -> sh' { ang  = sh'.ang + angVel'
                                                  , angVel = angVel' }
                    _ ->
                      let ang' = sh.ang + sh.angVel
                          absV  = abs sh.angVel - shipDrag
                          angVel' = if absV > 0.0 then absV * sign sh.angVel else 0.0
                      in  modShip s $ \sh' -> sh' { ang = ang', angVel = angVel' }
             -- update the blasters
            (s2 :: State) =
              let sh = s1.ship
              in  case s1.keys.fire of
                    KeyUp ->
                      modShip s1 $ \sh ->
                        sh { blasters = blastersContinue sh.blasters delta }
                    KeyDown ->
                      let rec =
                            if (L.length sh.blasters < maxBlasterBalls &&
                                blasterFarEnough sh.blasters)
                              then { bs:   { r: 0.0, ang: sh.ang } : sh.blasters
                                   , snds: FireSound : s1.soundEvents
                                   }
                              else { bs:   blastersContinue sh.blasters delta,
                                     snds: s1.soundEvents }
                      in s1 { ship = sh { blasters = rec.bs }
                            , soundEvents = rec.snds
                            }
            -- update the enemies
            (s3 :: State) =
              -- TODO: Refactor
              let res = map (updateWave s.time s.ship) s2.enemyWaves
                  ws' = map fst res
                  bombss = map snd res
              in  s2 { enemyWaves = ws', bombs = s2.bombs <> concat bombss }

            -- check for collisions with enemies
            (s4 :: State) =
               let sh = s3.ship
                   res = filterWave s.time
                            { ws: s3.enemyWaves
                            , ps: sh.blasters
                            , points: 0
                            }
               in  s3 { enemyWaves = res.ws
                      , ship = sh { blasters = res.ps }
                      , score = s3.score + res.points }
            -- check for bomb collisions
            (s5 :: State) =
               let collideB b = pos2IsectPos2 (shipPos s4.ship)
                                  shipRadius b.pos bombRadius
                   collision  = any id $ map collideB s.bombs
               in if collision then s4 { fsmState = GameOver } else s4
        in   updateTime delta
           $ updateStars delta
           $ updateBombs s5
      -- catch-all
      _ -> s
  where
    -- Returns 'Just (Tuple points es)' on a collision where
    -- 'es' is the remaining enemies and 'points' the score associated.
    -- Returns 'Nothing' on no collisions
    collish :: Time -> List Enemy -> Polar -> Maybe (Tuple Int (List Enemy))
    collish _ Nil _ = Nothing
    collish t (e:es) p =
      case enemyPos e t of
        Just enemyP3 ->
          let shipP   = polarToPos (actualBlasterPos p)
          in if pos2IsectPos3
                  shipP (blasterRadius p)
                  enemyP3 (scaleFactor enemyP3* enemyRadius)
             then -- collision. Remove the enemy. Return the rest
                  Just $ Tuple (enemyPoints e) es
             else consEnemy e $ collish t es p
        Nothing -> Nothing
      where
        consEnemy e' mbTup = case mbTup of
          Just (Tuple points es') -> Just $ Tuple points (e':es')
          Nothing                -> Nothing

    filterWave :: Time
        -> { ws :: List EnemyWave, ps :: List Polar, points :: Int }
        -> { ws :: List EnemyWave, ps :: List Polar, points :: Int }
    filterWave t { ws: Nil, ps: ps, points: points } = { ws: Nil, ps: ps, points: points }
    filterWave t { ws: (Cons wave waves), ps: ps, points: points } =
      let { es: es', ps: ps', points: points' } =
            if t >= wave.arriveTime
              then filterOnCollision t
                     { es: wave.enemies, ps: ps, points: points }
              else { es: wave.enemies, ps: ps, points: points }
      in  add (wave { enemies = es' }) $
            filterWave t { ws: waves, ps: ps', points: points' }
      where
        add w' { ws: ws',      ps: ps', points: points' } =
               { ws: (w':ws'), ps: ps', points: points' }

    --
    -- Filters enemies and blasters out on any collision.
    -- Stops at the first collision
    --
    filterOnCollision :: Time
                      -> { es :: List Enemy, ps :: List Polar, points :: Int }
                      -> { es :: List Enemy, ps :: List Polar, points :: Int }
    filterOnCollision _ rec@{ es: _, ps: Nil, points: _  } = rec
    filterOnCollision t { es: es, ps: p:ps, points: points} =
      case collish t es p of
        Just (Tuple points' es') -> { es: es', ps: ps, points: points' } -- collision. stop
        Nothing -> addPoint p (filterOnCollision t { es: es, ps: ps, points: points })
      where
        addPoint p' rec = rec { ps = p':rec.ps}

    modShip s' f = s' { ship = f s'.ship }
    modShipVel s' f =
      modShip s' $ \sh ->
        let angVel' = clamp (-shipMaxVel) (f sh.angVel) shipMaxVel
        in  sh { angVel = angVel' }
    modKeys s' f = s' { keys = f s'.keys }

    blastersContinue :: List Polar -> Number -> List Polar
    blastersContinue balls delta = catMaybes $ map updateBlasterBall balls
      where
        updateBlasterBall pp =
          if pp.r > shipCircleRadius
            then Nothing
            else Just { r: pp.r + blasterVel*delta, ang: pp.ang }

    -- checks if the closest blaster ball (if any) is far enough away
    blasterFarEnough :: List Polar -> Boolean
    blasterFarEnough balls =
      case balls of
        Nil   -> true
        (b:_) -> b.r > blasterRechargeDistance


    updateTime delta s' = s' { time = s'.time + delta }
    updateStars delta s' = rec.s { starField = rec.newStars }
      where
        rec = foldl step { s: s', newStars: LL.nil} s'.starField
        step :: { s :: State, newStars :: LL.List Star } -> Star
             -> { s :: State, newStars :: LL.List Star }
        step rec' star =
          if star.r > maxStarR
            then { s:        rec'.s { starCollectionIdx = (rec'.s.starCollectionIdx + 1) `mod` generatedStars }
                 , newStars: LL.snoc rec'.newStars (unsafePartial (fromJust (rec'.s.starCollection LL.!! rec'.s.starCollectionIdx))) }
            else { s:        rec'.s
                 , newStars: LL.snoc rec'.newStars (star { r = star.r + star.r/maxStarR*delta*star.vel }) }

    updateWave :: Time -> Ship -> EnemyWave -> Tuple EnemyWave (List Bomb)
    updateWave t ship wave =
      let enemies' = catMaybes $ map (updateEnemy wave.arriveTime t)
                                        wave.enemies
          Tuple enemies'' mbBombs =
            unzip $ map (maybeReleaseBomb wave.arriveTime t ship) enemies'
      in Tuple wave { enemies = enemies''} (catMaybes mbBombs)

    -- `Nothing` means enemy should be removed
    updateEnemy :: Time -> Time -> Enemy -> Maybe Enemy
    updateEnemy arriveTime t en =
      case t >= arriveTime + en.delta of
        true ->
          case en.startedAt of
            Nothing -> Just $ en { startedAt = Just t }
            Just startedAt ->
              let pathDuration = (en.pathSegments !!! en.index).duration
              in case t > startedAt + pathDuration of
                   true ->
                     if en.index == length en.pathSegments - 1
                       then Nothing
                       else Just $
                              en { startedAt = Just t
                                 , mbReleaseBombAt =
                                     (\x -> x - pathDuration) <$>
                                       en.mbReleaseBombAt
                                 ,  index = en.index + 1 }
                   false -> Just en -- leave it alone
        false -> Just en -- leave it alone
    maybeReleaseBomb :: Time -> Time -> Ship -> Enemy -> Tuple Enemy (Maybe Bomb)
    maybeReleaseBomb arriveTime t ship en =
      case Tuple en.mbReleaseBombAt (enemyPos en t) of
        Tuple (Just t') (Just epos) ->
          if t > t' + arriveTime + en.delta
            then let pos = { x: epos.x, y: epos.y }
                     bomb = Just { pos: pos
                                 , dir: dirBetweenPoints pos (shipPos ship) }
                 in  Tuple (en { mbReleaseBombAt = Nothing }) bomb
            else Tuple en Nothing
        _ -> Tuple en Nothing
    updateBombs :: State -> State
    updateBombs s' = s' { bombs = catMaybes $ map updateBomb s'.bombs }
      where
        updateBomb :: Bomb -> Maybe Bomb
        updateBomb b =
          if outOfBounds b.pos
            then Nothing
            else
              let pos' = { x: b.pos.x + b.dir.x * bombVel
                         , y: b.pos.y + b.dir.y * bombVel
                         }
              in  Just $ b { pos = pos' }

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
  void $ modifySTRef st $ \s -> updateLevel (toMsg a) s
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
          void $ modifySTRef st $ \s -> updateLevel (toMsg { x: x, y: y }) s
          pure unit
        Left _ -> pure unit


setupMainLoop
  :: forall s eff. STRef s State
  -> Eff (canvas :: CANVAS
         , random :: RANDOM
         , st :: ST s
         , timer :: TIMER | eff) Unit
setupMainLoop st = do
  void $ setInterval (Int.floor (1000.0/framesPerSecond)) $ do
    s <- readSTRef st
    -- This is where the Finite State Machine is controlled from
    case s.fsmState of
      Level -> do
        renderLevel st
        void $ modifySTRef st $ \s' -> updateLevel (Tick (1.0/framesPerSecond)) s'
      GameOver -> do
        renderGameOver st

keydown :: String -> Msg
keydown code =
  case code of
     "Space"      -> Fire          KeyDown
     "ArrowLeft"  -> Clockwise     KeyDown
     "ArrowRight" -> Anticlockwise KeyDown
     _  -> NoOp

keyup :: String -> Msg
keyup code =
  case code of
     "Space"      -> Fire          KeyUp
     "ArrowLeft"  -> Clockwise     KeyUp
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

mouseToWorld :: { x :: Int, y :: Int } -> Size -> Vec2
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