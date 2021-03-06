module Gyruss.Render (
  module Graphics.Canvas,
  module Gyruss.Render

) where

import Graphics.Canvas

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.ST (ST, STRef, modifySTRef, readSTRef)
import Data.Int as Int
import Data.List (List(..), concatMap)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_, traverse)
import Gyruss.Types (Enemy, State, Time, Bomb, enemyRadius, maxStarR, shipCircleRadius, starRadius, worldWidth, bombRadius)
import Gyruss.Util (blasterRadius, scaleFactor, shipPos, enemyPos)
import Math (cos, pi, sin, floor)
import Prelude (Unit, bind, discard, min, negate, pure, show, unit, void, ($), (*), (+), (/), (<>))

renderGameOver :: forall s e. STRef s State
               -> Eff (st :: ST s, canvas :: CANVAS| e) Unit
renderGameOver st = do
  s <- readSTRef st
  clearScreen s
  toScreen s $ do
    let ctx = s.context2D
        fontSize = floor (30.0 / ((min s.screenSize.w s.screenSize.h) / worldWidth))
    void $ scale { scaleX: 1.0, scaleY: -1.0 } ctx
    void $ setFont (show fontSize <> "px Helvetica") ctx
    void $ setFillStyle "white" ctx
    void $ setTextAlign ctx AlignCenter
    void $ fillText ctx "Game Over" 0.0 0.0


renderLevel :: forall s e. STRef s State
       -> Eff (st :: ST s, random :: RANDOM
              {-, wau :: WebAudio-}, canvas :: CANVAS| e) Unit
renderLevel st = do
  s <- readSTRef st
  let sz = s.screenSize
  -- FIXME: setting the canvas size every time is just dumb!
  void $ setCanvasWidth  sz.w s.canvas
  void $ setCanvasHeight sz.h s.canvas
  clearScreen s
  toScreen s $ do
    renderStars s
    renderShip s "#ffffff"
    renderEnemies s
    renderBombs s
    renderScore s
--  traverse (playSoundEvent s.sounds) s.soundEvents
  void $ modifySTRef st $ \s' -> s' { soundEvents = Nil }
  pure unit

-- playSoundEvent :: forall e. Sounds -> SoundEvent -> Eff (wau :: WebAudio | e) Unit
-- playSoundEvent sounds ev =
--   case ev of
--     FireSound -> playBufferedSound sounds sounds.fireBuffer

clearScreen :: forall e. State -> Eff (canvas :: CANVAS | e) Unit
clearScreen s = do
  let ctx = s.context2D
  void $ setFillStyle "#000000" ctx
  void $ fillPath ctx $ rect ctx { x: 0.0, y: 0.0
                                 , w: s.screenSize.w, h: s.screenSize.h }



renderScore :: forall e. State -> Eff (canvas :: CANVAS | e) Unit
renderScore s = do
  let ctx = s.context2D
      fontSize = floor (30.0 / ((min s.screenSize.w s.screenSize.h) / worldWidth))
  void $ scale { scaleX: 1.0, scaleY: yFactor } ctx
  void $ setFont (show fontSize <> "px Helvetica") ctx
  void $ setFillStyle "yellow" ctx
  void $ fillText ctx  (show s.score) (-worldWidth/4.0) (yFactor*(0.45*worldWidth))
  where
    yFactor = -1.0

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
    traverse_ (renderBlasterBall ctx) s.ship.blasters
    pure unit
  where
    renderBlasterBall ctx pp = do
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

renderEnemies :: forall e. State -> Eff ( canvas :: CANVAS | e ) Unit
renderEnemies s =
  traverse_ (renderEnemy s.context2D s.time) $
   concatMap (\w -> w.enemies) s.enemyWaves

renderEnemy :: forall e. Context2D -> Time -> Enemy
            -> Eff ( canvas :: CANVAS | e ) Unit
renderEnemy ctx t en = do
  case enemyPos en t of
    Just p -> do
      void $ save ctx
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
    Nothing -> pure unit

renderBombs :: forall e. State -> Eff (canvas :: CANVAS | e) Unit
renderBombs s =
  traverse_ (renderBomb s.context2D) $ s.bombs

renderBomb :: forall e. Context2D -> Bomb -> Eff (canvas :: CANVAS | e) Unit
renderBomb ctx b = do
  void $ save ctx
  void $ beginPath ctx
  void $ setFillStyle "rgba(255,255,0)" ctx -- yellow
  void $ fillCircle ctx { x: b.pos.x, y: b.pos.y, r: bombRadius }
  void $ fill ctx
  void $ restore ctx

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


fillCircle :: forall e. Context2D
           -> { x :: Number, y :: Number, r :: Number }
           -> Eff (canvas :: CANVAS | e) Unit
fillCircle ctx o = fillPath ctx $ circlePath ctx o


circlePath :: forall e. Context2D ->  { x :: Number, y :: Number, r :: Number }
           -> Eff (canvas :: CANVAS | e) Unit
circlePath ctx o = do
  void $ arc ctx
           { x:     o.x
           , y:     o.y
           , r:     o.r
           , start: 0.0
           , end:   2.0*pi
           }
  pure unit

colorStr :: Int -> Int -> Int -> String
colorStr r g b = "rgb("<>show r<>","<>show g<>","<>show b<>")"
