module Graphics.Triangle
  (
    Graphics.Triangle.isInTriangle
  , Graphics.Triangle.isInCircle
  , Triangle
  , makeTriangle
  , Graphics.Triangle.show
  )
  where

import Graphics.Point
import Graphics.Triangle.Internal

import Data.Matrix
import Debug.Trace as T
import Data.Set as Set

  -- -------------------------------------------  "Triangle" data structure  -------------------------------------------
  
-- | A set of exactly three points.
data Triangle = Triangle (Set Point) -- We use Set so we get Eq and intersection ops for free (intersection for deciding
                                     -- edge comonality).

-- | Makes a Triangle from three points.
makeTriangle :: Point -> Point -> Point -> Triangle
makeTriangle a b c = Triangle (Set.fromList [a,b,c])

show :: Triangle -> String
show (Triangle pts) = "(" ++ (Prelude.show a) ++ ", " ++ (Prelude.show b) ++ ", " ++ (Prelude.show c) ++ ")"
  where [a,b,c] = Set.toList pts

  -- ----------------------------------------  "Triangle" data structure ends  -----------------------------------------

-- | Returns True iff the given point is inside the given non-degenerate triangle.
-- | Makes a fast, accurate determination by combining the other (private) functions in this module.
isInTriangle :: Double -> Point -> Point -> Point -> Point -> Bool
isInTriangle
  epsilon                       -- ^ If point is within this distance of an edge of the triangle or its bounding box, it's in
  pt                            -- ^ Point under test
  pt1                           -- ^ First triangle vertex  
  pt2                           -- ^ Second triangle vertex 
  pt3                           -- ^ Third triangle vertex  
  = if not $ isPointInTriangleBoundingBox epsilon pt pt1 pt2 pt3     then -- T.trace "Not in bounding box"
                                                                          False
    else if isPointInTriangleNaive pt pt1 pt2 pt3                    then -- T.trace "Is in triangle naively"
                                                                          True
         else if isWithinEpsilonOf epsilon pt pt1 pt2
                 || isWithinEpsilonOf epsilon pt pt2 pt3
                 || isWithinEpsilonOf epsilon pt pt3 pt1             then -- T.trace ("Is within " ++ show epsilon ++ " of a side")
                                                                          True
                                                                     else -- T.trace "Not in triangle"
                                                                          False

-- | Returns true iff the given point is in the circle determined by the given triangle.  Note that this function will
-- coerce the triangle to be counterclockwise (by testing it and transposing the last two points if necessary).
isInCircle :: Point -> Point -> Point -> Point -> Bool
isInCircle
  ptA @ (Point _ (x_A, y_A))    -- ^ 1st point of triangle
  ptB @ (Point _ (x_B, y_B))    -- ^ 2nd point of triangle
  ptC @ (Point _ (x_C, y_C))    -- ^ 3rd point of triangle
  ptD @ (Point _ (x_D, y_D))    -- ^ Point being tested
  =
{-
   True iff the determinant of 4x4 matrix is positive.

   | x_A   y_A   x_A^2 + y_A^2     1 |
   | x_B   y_B   x_B^2 + y_B^2     1 | > 0
   | x_C   y_C   x_C^2 + y_C^2     1 |
   | x_D   y_D   x_D^2 + y_D^2     1 |
-}
  if isCounterClockwise ptA ptB ptC
  then 0 < (detLU $ fromLists [ [ x_A, y_A, x_A^2 + y_A^2, 1]
                              , [ x_B, y_B, x_B^2 + y_B^2, 1]
                              , [ x_C, y_C, x_C^2 + y_C^2, 1]
                              , [ x_D, y_D, x_D^2 + y_D^2, 1]
                              ])
  else 0 < (detLU $ fromLists [ [ x_A, y_A, x_A^2 + y_A^2, 1]
                              , [ x_C, y_C, x_C^2 + y_C^2, 1] -- Transpose B & C
                              , [ x_B, y_B, x_B^2 + y_B^2, 1]
                              , [ x_D, y_D, x_D^2 + y_D^2, 1]
                              ])

  
