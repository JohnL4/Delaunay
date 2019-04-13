module Graphics.Triangle
  (
    Graphics.Triangle.isIn
  )
  where

import Graphics.Point
import Graphics.Triangle.Internal

{-
epsilon :: Double
epsilon = 0.001

epsilonSqrd :: Double
epsilonSqrd = epsilon * epsilon
-}

-- | Returns True iff the given point is inside the given non-degenerate triangle.
-- | Makes a fast, accurate determination by combining the other (private) functions in this module.
isIn :: Double -> Point -> Point -> Point -> Point -> Bool
isIn
  epsilon                       -- ^ If point is within this distance of an edge of the triangle or its bounding box, it's in
  pt                            -- ^ Point under test
  pt1                           -- ^ First triangle vertex  
  pt2                           -- ^ Second triangle vertex 
  pt3                           -- ^ Third triangle vertex  
  = if not $ isPointInTriangleBoundingBox epsilon pt pt1 pt2 pt3        then False
    else if isPointInTriangleNaive pt pt1 pt2 pt3                       then True
         else if isWithinEpsilonOf epsilon pt pt1 pt2
                 || isWithinEpsilonOf epsilon pt pt2 pt3
                 || isWithinEpsilonOf epsilon pt pt3 pt1                  then True
                                                                        else False

{-
    function accuratePointInTriangle(x1, y1, x2, y2, x3, y3, x, y:Number):Boolean
    {
     if (! pointInTriangleBoundingBox(x1, y1, x2, y2, x3, y3, x, y))
      return false;

     if (naivePointInTriangle(x1, y1, x2, y2, x3, y3, x, y))
      return true;

     if (distanceSquarePointToSegment(x1, y1, x2, y2, x, y) <= EPSILON_SQUARE)
      return true;
     if (distanceSquarePointToSegment(x2, y2, x3, y3, x, y) <= EPSILON_SQUARE)
      return true;
     if (distanceSquarePointToSegment(x3, y3, x1, y1, x, y) <= EPSILON_SQUARE)
      return true;

     return false;
    }
-}

