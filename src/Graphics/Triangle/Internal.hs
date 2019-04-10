module Graphics.Triangle.Internal where

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
  = (y2 - y1) * (x - x1) + (-x2 * x1) * (y - y1) -- TODO: not sure the sign of this is correct. Totologic may have had
                                                 -- the y-axis flipped.

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
  pt                            -- ^ Point under test
  pt1                           -- ^ First triangle vertex  
  pt2                           -- ^ Second triangle vertex 
  pt3                           -- ^ Third triangle vertex  
  = False -- TODO: implement

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

-- | Computes square of distance from given point to given side.
distanceSqrdPointToSide :: Point -> Point -> Point -> Double
distanceSqrdPointToSide
  pt                            -- ^ The point under test
  pt1                           -- ^ Starting point of side
  pt2                           -- ^ Ending point of side
  = 0.5 -- TODO: implement

{-
    function distanceSquarePointToSegment(x1, y1, x2, y2, x, y:Number):Number
    {
     var p1_p2_squareLength:Number = (x2 - x1)*(x2 - x1) + (y2 - y1)*(y2 - y1);
     var dotProduct:Number = ((x - x1)*(x2 - x1) + (y - y1)*(y2 - y1)) / p1_p2_squareLength;
     if ( dotProduct < 0 )
     {
      return (x - x1)*(x - x1) + (y - y1)*(y - y1);
     }
     else if ( dotProduct <= 1 )
     {
      var p_p1_squareLength:Number = (x1 - x)*(x1 - x) + (y1 - y)*(y1 - y);
      return p_p1_squareLength - dotProduct * dotProduct * p1_p2_squareLength;
     }
     else
     {
      return (x - x2)*(x - x2) + (y - y2)*(y - y2);
     }
    }
-}
