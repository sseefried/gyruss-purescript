module Gyruss.Util where

import Prelude ((*), (+), (-), (/), (<))

import Gyruss.Types (Polar, Pos, Pos3, Ship, screenDist, shipCircleRadius)
import Math (cos, sin)


--
-- How much to scale the x,y co-ordinates of an object that is a certain
-- distance away in the z axis.
--
scaleFactor :: Pos3 -> Number
scaleFactor p3 = screenDist / (screenDist - p3.z)

shipPos :: Ship -> Pos
shipPos ship = { x:  shipCircleRadius*cos ship.ang
               , y:  shipCircleRadius*sin ship.ang }


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


