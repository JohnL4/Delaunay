import Test.Hspec

import Graphics.Delaunay
import Graphics.Point as Pt
import Graphics.MeshPoint as MP
import Graphics.Graph as G

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

{-
Initial result of dumb 4-pt delTri call:

Graph (fromList [
          MeshPoint {point = Point {id = 0, coords = (1.0,1.0)},
                     adj = fromList [Point {id = 1, coords = (2.0,2.0)}
                                    ,Point {id = 2, coords = (3.0,3.0)}]}
          ,MeshPoint {point = Point {id = 1, coords = (2.0,2.0)},
                      adj = fromList [Point {id = 0, coords = (1.0,1.0)}
                                     ,Point {id = 2, coords = (3.0,3.0)}]}
          ,MeshPoint {point = Point {id = 2, coords = (3.0,3.0)},
                      adj = fromList [Point {id = 0, coords = (1.0,1.0)}
                                     ,Point {id = 1, coords = (2.0,2.0)}]}
          ,MeshPoint {point = Point {id = 3, coords = (4.0,4.0)},
                      adj = fromList []}]
      )
-}



{-
main = do
  putStrLn "----------------  Begin Test  ----------------"
  delaunayIO
  putStrLn "Test suite not yet implemented"
-}
