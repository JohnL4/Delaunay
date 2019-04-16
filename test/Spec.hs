import Test.Hspec

import Graphics.Delaunay
import Graphics.Point as Pt
import Graphics.MeshPoint as MP
import Graphics.Graph as G
import Graphics.Triangle
import Graphics.Triangle.Internal
import TriTree

import Data.Set

main :: IO ()
main = hspec $ do
  describe "Basics/throat-clearing" $ do
    it "Processes an empty list of points" $
      delTri [] == Graph empty
    it "Returns empty set if there are fewer than three points" $
      delTri [ (1.0, 1.0) ] == Graph empty
      &&
      delTri [ (1.0, 1.0), (1.0, 1.0) ] == Graph empty
    it "Returns a very simple when given three points" $
      (let
{-
          a
         / \
        /   \
       b-----c
-}
          aCoords = (2.0, 2.0)
          bCoords = (1.0, 1.0)
          cCoords = (3.0, 1.0)
          a       = Point 0 aCoords
          b       = Point 1 bCoords
          c       = Point 2 cCoords
       in
          delTri [ aCoords, bCoords, cCoords ]
          == Graph (fromList [ meshPoint a [b,c], -- Use of Sets (not Lists) means we don't have to get the order right.  Yay!
                               meshPoint b [a,c],
                               meshPoint c [a,b]
                             ])
      )
  describe "Adding a single point to an existing triangulation" $ do
    it "Returns a 4-point Graph when called with 4 coordinate pairs" $
      G.size (delTri [
{-
          a-----d
         /     /
        /     /
       b-----c
-}
               (2.0, 2.0)
               , (1.0, 1.0)
               , (3.0, 1.0)
               , (4.0, 2.0)
               ]) == 4
  describe "Graphics.Triangle.Internal.dotProduct" $ do
{-
                        b
                    t  /
                      /  u
                     a
-}
    it "Returns correct sign" $
      let a = Point 1 (1,1)
          b = Point 2 (3,3)
          t = Point 3 (0,2)
          u = Point 4 (4,2)
      in dotProduct t a b > 0
         && dotProduct u a b < 0
         -- Reversed endpoints should reverse signs (duh?)
         && dotProduct t b a < 0
         && dotProduct u b a > 0

  describe "Graphics.Triangle.Internal.isPointInTriangleBoundingBox" $ do
{-
                       v

                   +---c---+
                   |  / \ s|
               w   | / t \ |  u
                   |/     \|
                   a-------b
                       r
-}
    it "Returns correct value" $
      let a = Point 1 (2,2)
          b = Point 2 (6,2)
          c = Point 3 (4,6)
          t = Point 4 (4,4)
          u = Point 5 (8,4)
          v = Point 6 (4,8)
          w = Point 7 (0,4)
          r = Point 8 (4,0)
          s = Point 9 (5.9,5.9)
      in (Prelude.map (\pt -> isPointInTriangleBoundingBox 0.001 pt a b c) [t,u,v,w,r,s])
         == [True, False, False, False, False, True]

  describe "Graphics.Triangle.Internal.isPointInTriangleNaive" $ do
    it "Is True for point inside triangle" $
      isPointInTriangleNaive  (Point 99 (0.2, 0.2))
                              (Point 1 (1,0))
                              (Point 2 (cos (2 * pi / 3), sin (2 * pi / 3)))  
                              (Point 3 (cos (4 * pi / 3), sin (4 * pi / 3)))  
    it "Is True no matter the order of the triangle vertices" $
      isPointInTriangleNaive  (Point 99 (0.2, 0.2))
                              (Point 1 (1,0))
                              (Point 3 (cos (4 * pi / 3), sin (4 * pi / 3)))  
                              (Point 2 (cos (2 * pi / 3), sin (2 * pi / 3)))  
    it "Is False for point outside triangle" $
      not $ isPointInTriangleNaive  (Point 99 (2,0))
                                    (Point 1 (1,0))
                                    (Point 2 (cos (2 * pi / 3), sin (2 * pi / 3)))  
                                    (Point 3 (cos (4 * pi / 3), sin (4 * pi / 3)))  
    it "Is False for point outside triangle regardless of vertex order" $
      not $ isPointInTriangleNaive  (Point 99 (2,0))
                                    (Point 1 (1,0))
                                    (Point 3 (cos (4 * pi / 3), sin (4 * pi / 3)))  
                                    (Point 2 (cos (2 * pi / 3), sin (2 * pi / 3)))  

  describe "Graphics.Triangle.Internal.isWithinEpsilonOf" $ do
    it "is false for points way off p1" $
      not $ isWithinEpsilonOf 0.2 (Point 99 (0,0)) (Point 1 (1,1)) (Point 2 (2,2))
    it "is false for points way off the midpt" $
      not $ isWithinEpsilonOf 0.2 (Point 99 (1.75, 1.25)) (Point 1 (1,1)) (Point 2 (2,2))
    it "is false for points way off p2" $
      not $ isWithinEpsilonOf 0.2 (Point 99 (3,3)) (Point 1 (1,1)) (Point 2 (2,2))
    it "is true for points just barely off p1" $
      isWithinEpsilonOf 0.2 (Point 99 (0.9, 0.9)) (Point 1 (1,1)) (Point 2 (2,2))
    it "is true for points just barely to the right of the midpt" $
      isWithinEpsilonOf 0.2 (Point 99 (1.6, 1.5)) (Point 1 (1,1)) (Point 2 (2,2))
    it "is true for points just barely below the midpt" $
      isWithinEpsilonOf 0.2 (Point 99 (1.5, 1.4)) (Point 1 (1,1)) (Point 2 (2,2))
    it "is true for points just barely off p2" $
      isWithinEpsilonOf 0.2 (Point 99 (2.1, 2.1)) (Point 1 (1,1)) (Point 2 (2,2))

  describe "Graphics.Triangle.Internal.isCounterClockwise" $ do
    it "Is true for simple triangles" $
      isCounterClockwise (Point 1 (1,1)) (Point 2 (2,1)) (Point 3 (2,2))
    it "Is true for counterclockwise triangles defined with trig" $
      not $ isCounterClockwise   (Point 1 (cos (pi/3), sin (pi/3)))  
                                 (Point 2 (1,0))                     
                                 (Point 3 (cos (-pi/3), sin (-pi/3)))
    it "Is false for clockwise triangles defined with trig" $
      isCounterClockwise   (Point 1 (cos (pi/3), sin (pi/3)))  
                           (Point 3 (cos (-pi/3), sin (-pi/3)))
                           (Point 2 (1,0))                     

  describe "Graphics.Triangle.Internal.isInCircle" $ do
    it "Returns false for points outside an equilateral triangle" $
      not $ isInCircle  (Point 1 (1,0))
                        (Point 2 (cos (2 * pi / 3), sin (2 * pi / 3)))
                        (Point 3 (cos (4 * pi / 3), sin (4 * pi / 3)))
                        (Point 99 (2,0))                                
    it "Returns true for points inside an equilateral triangle" $
      isInCircle  (Point 1 (1,0))                               
                  (Point 2 (cos (2 * pi / 3), sin (2 * pi / 3)))
                  (Point 3 (cos (4 * pi / 3), sin (4 * pi / 3)))
                  (Point 99 (0.2, 0.2))
    it "Returns false for points outside the circle described by an arc" $
      not $ isInCircle  (Point 1 (cos (pi/3), sin (pi/3)))
                        (Point 2 (cos (-pi/3), sin (-pi/3)))
                        (Point 3 (1,0))
                        (Point 99 (2,0))
    it "Doesn't care about the order of the points" $
      not $ isInCircle  (Point 1 (cos (pi/3), sin (pi/3)))
                        (Point 2 (1,0))
                        (Point 3 (cos (-pi/3), sin (-pi/3)))
                        (Point 99 (2,0))
    it "Returns true for points inside the circle described by an arc" $
      isInCircle  (Point 1 (cos (pi/3), sin (pi/3)))
                  (Point 2 (1,0))
                  (Point 3 (cos (-pi/3), sin (-pi/3)))
                  (Point 99 (0.2, 0.2))

  describe "Graphics.Triangle.isInTriangle" $ do
    it "Returns true for the simple case" $
      isInTriangle 0.001   (Point 99 (0.4, 0.2))
                           (Point 1 (0,0))
                           (Point 2 (1,0))
                           (Point 3 (1,1))
    it "Returns true regardless of the order of triangle points" $
      isInTriangle 0.001   (Point 99 (0.4, 0.2))
                           (Point 1 (0,0))
                           (Point 3 (1,1))
                           (Point 2 (1,0))

  describe "TriTree" $ do
    it "adds point" $
      addPoint (TriTree { triangle = Data.Set.empty }) (Point 1 (1,1)) == Data.Set.empty
    it "flips edge" $
      flipEdge (TriTree { triangle = Data.Set.empty }) (TriTree { triangle = Data.Set.empty })
      == ( TriTree { triangle = Data.Set.empty }, TriTree { triangle = Data.Set.empty } )
