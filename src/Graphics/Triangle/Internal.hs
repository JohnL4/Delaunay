module Graphics.Triangle.Internal where

import Debug.Trace
import Data.Matrix

import Graphics.Point

-- | Compute the dot product of two vectors:
-- (1) The vector that is "left-orthogonal" to the given side;
-- (2) The vector from the beginning of the given side to the point being tested.
-- The order in which the points are given is irrelevant; see the totologic comments file in the source code.
dotProduct :: Point -> Point -> Point -> Double
dotProduct
  (Point id (x,y))              -- ^ The point being tested.
  (Point id1 (x1,y1))           -- ^ Starting point of the side
  (Point id2 (x2,y2))           -- ^ Ending point of the side
  = (y1 - y2) * (x - x1) + (x2 - x1) * (y - y1)

{-
Totologic ActionScript 3 code:

    const EPSILON:Number = 0.001; // Not sure how we come up with this value.
    const EPSILON_SQUARE:Number = EPSILON*EPSILON;

    /**
     ,* Originally named "side", but this is actually the dot product between two vectors:
     ,* 1) The vector that is left-orthogonal to the given side (p1 -> p2) (TODO: make sure this arrow goes the right way)
     ,* 2) The vector from the starting point of the side to the point being tested.
     ,*/
    function dotProductWithSideOrthogonal(x1, y1, x2, y2, x, y:Number):Number
    {
     return (y2 - y1)*(x - x1) + (-x2 + x1)*(y - y1);
    }
-}

-- | Naive approach to determine whether the given point is in the given triangle (uses dot product).
isPointInTriangleNaive :: Point -> Point -> Point -> Point -> Bool
isPointInTriangleNaive
  pt                            -- ^ Point under test
  pt1                           -- ^ First triangle vertex
  pt2                           -- ^ Second triangle vertex
  pt3                           -- ^ Third triangle vertex
  = False -- TODO: implement

{-
    /**
     ,* "Naive" because when the point is extremely close to (or exactly on) a side, the dot product can give spurious
     ,* results.
     ,*/
    function naivePointInTriangle(x1, y1, x2, y2, x3, y3, x, y:Number):Boolean
    {
     var checkSide1:Boolean = dotProductWithSideOrthogonal(x1, y1, x2, y2, x, y) >= 0;
     var checkSide2:Boolean = dotProductWithSideOrthogonal(x2, y2, x3, y3, x, y) >= 0;
     var checkSide3:Boolean = dotProductWithSideOrthogonal(x3, y3, x1, y1, x, y) >= 0;
     return checkSide1 && checkSide2 && checkSide3;
    }
-}

-- | Returns true if the given point is inside the triangle's rectangular bounding box.  (Permits fast ruling-out of
-- point in triangle.)
isPointInTriangleBoundingBox :: Double -> Point -> Point -> Point -> Point -> Bool
isPointInTriangleBoundingBox
  epsilon                       -- ^ Error amount, point must be w/in this distance of box edge (or clearly inside the
                                -- box) to count as "in".
  (Point _ (x,y))               -- ^ Point under test
  (Point _ (x1,y1))             -- ^ First triangle vertex
  (Point _ (x2,y2))             -- ^ Second triangle vertex
  (Point _ (x3,y3))             -- ^ Third triangle vertex
  = xMin <= x && x <= xMax && yMin <= y && y <= yMax
  where
    xMin = minimum [x1, x2, x3] - epsilon
    xMax = maximum [x1, x2, x3] + epsilon
    yMin = minimum [y1, y2, y3] - epsilon
    yMax = maximum [y1, y2, y3] + epsilon

{-
    function pointInTriangleBoundingBox(x1, y1, x2, y2, x3, y3, x, y:Number):Boolean
    {
     var xMin:Number = Math.min(x1, Math.min(x2, x3)) - EPSILON;
     var xMax:Number = Math.max(x1, Math.max(x2, x3)) + EPSILON;
     var yMin:Number = Math.min(y1, Math.min(y2, y3)) - EPSILON;
     var yMax:Number = Math.max(y1, Math.max(y2, y3)) + EPSILON;

     if ( x < xMin || xMax < x || y < yMin || yMax < y )
      return false;
     else
      return true;
    }
-}

-- | Returns True iff the given point is within the given distance epsilon of the given side
isWithinEpsilonOf :: Double -> Point -> Point -> Point -> Bool
isWithinEpsilonOf
  epsilon                       -- ^ The allowed absolute error
  (Point _ (x,y))               -- ^ The point under test
  (Point _ (x1,y1))             -- ^ Starting point of side
  (Point _ (x2,y2))             -- ^ Ending point of side
  = let p1p2LengthSqrd  = (x2-x1) * (x2-x1) + (y2-y1) * (y2-y1) -- Length of p1p2, squared.
        p1pLengthSqrd   = (x1-x) * (x1-x) + (y1-y) * (y1-y)     -- Length of p1p, squared
        p2pLengthSqrd   = (x2-x) * (x2-x) + (y2-y) * (y2-y)     -- Length of p2p, squared
        dotProduct      = ((x-x1) * (x2-x1) + (y-y1) * (y2-y1)) -- Dot product of p1p2 and p1p
        epsilonSqrd     = epsilon * epsilon
    in
      if dotProduct < 0 then
        -- trace ("dotProduct < 0, p1pLengthSqrd = " ++ show p1pLengthSqrd)
        p1pLengthSqrd <= epsilonSqrd -- Angle > 90°, closest point of side IS p1
      else if dotProduct * dotProduct <= p1p2LengthSqrd * p1p2LengthSqrd -- See accompanying Jupyter notebook
           then -- trace "p off side"
                p1p2LengthSqrd * p1pLengthSqrd
                - ((x-x1) * (x1-x2) + (y-y1) * (y1-y2)) * ((x-x1) * (x1-x2) + (y-y1) * (y1-y2))
                <= epsilonSqrd * p1p2LengthSqrd
           else
             -- trace "p off p2"
             p2pLengthSqrd <= epsilonSqrd -- Project is past p2, closest point of side IS p2

-- | Returns True iff the points given are in counterclockwise order.  (Assumes they're not colinear or coincident.)
isCounterClockwise :: Point -> Point -> Point -> Bool
isCounterClockwise (Point _ (x_A, y_A)) (Point _ (x_B, y_B)) (Point _ (x_C, y_C))
  = 0 < (detLU $ fromLists [ [ x_A, y_A, 1 ]
                           , [ x_B, y_B, 1 ]
                           , [ x_C, y_C, 1 ]
                           ])
    
