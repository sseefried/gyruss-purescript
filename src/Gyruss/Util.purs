module Gyruss.Util where


import Gyruss.Types (Polar, Vec2, Vec3, Pos2, Pos3, Ship, Enemy, Time
                    , EnemySort(..), screenDist, shipCircleRadius
                    , worldWidth)
import Data.Array ((!!))
import Data.Maybe (fromJust, Maybe(..))
import Partial.Unsafe (unsafePartial)
import Prelude ((*), (+), (-), (/), (<), ($), (>), (||))
import Math (cos, sin, sqrt, abs)


--
-- How much to scale the x,y co-ordinates of an object that is a certain
-- distance away in the z axis.
--
scaleFactor :: Vec3 -> Number
scaleFactor p3 = screenDist / (screenDist - p3.z)

shipPos :: Ship -> Vec2
shipPos ship = { x: shipCircleRadius*cos ship.ang
               , y: shipCircleRadius*sin ship.ang
               }


polarToPos :: Polar -> Vec2
polarToPos pp = { x: pp.r * cos pp.ang, y: pp.r * sin pp.ang }

actualBlasterPos :: Polar -> Polar
actualBlasterPos pp = { r: shipCircleRadius - pp.r, ang: pp.ang }

--
-- Given the position of the blaster (relative to ship) returns the
-- radius of the blaster ball
--
blasterRadius :: Polar -> Number
blasterRadius pp = 0.5 + 0.5*(1.0 - pp.r/shipCircleRadius)

--
-- Checks whether Vec2 intersects with Vec3 within a certain radius
--
pos2IsectPos3 :: Vec2 -> Number -> Vec3 -> Number -> Boolean
pos2IsectPos3 p r p3 r' =
  let dx = p.x - p3.x
      dy = p.y - p3.y
      d  = r + r'
  in dx*dx + dy*dy < d*d

-- Unsafe version of (!!)
infix 5 unsafeIndex as !!!

unsafeIndex :: forall a. Array a -> Int -> a
unsafeIndex a i = unsafePartial $ fromJust $ a !! i

--
-- Calculate the position of an enemy
--
enemyPos :: Enemy -> Time -> Maybe Vec3
enemyPos en worldTime =
  case en.startedAt of
    Just startedAt' ->
      let seg = en.pathSegments !!! en.index
          t   = worldTime - startedAt'
          t'  = t * (seg.funcFinish - seg.funcStart)/seg.duration + seg.funcStart
      in Just $ seg.func t'
    Nothing -> Nothing

enemyPoints :: Enemy -> Int
enemyPoints en =
  case en.sort of
    Normal -> 50

toUnitVector :: Vec2 -> Vec2
toUnitVector { x: x, y: y } =  { x: x/d, y: y/d }
  where
    d = sqrt (x*x + y*y)


--
-- Calculates a distance vector between two points
--
dirBetweenPoints :: Vec2 -> Vec2 -> Vec2
dirBetweenPoints v1 v2 = toUnitVector { x: v2.x - v1.x, y: v2.y - v1.y }


outOfBounds :: Vec2 -> Boolean
outOfBounds { x: x, y: y } = abs x > d || abs y > d
  where
    d= worldWidth/2.0