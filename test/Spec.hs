import Test.Hspec

import Graphics.Delaunay
import Graphics.Point as Pt
import Graphics.MeshPoint as MP
import Graphics.Graph

import Data.Set

main :: IO ()
main = hspec $ do
  describe "Basics/throat-clearing" $ do
    it "Processes an empty list of points" $
      delaunayTriangulationOf [] == Graph empty
    it "Returns empty set if there are fewer than three points" $
      delaunayTriangulationOf [ Point { Pt.id = 1, coords = (1.0, 1.0) } ] == Graph empty
      &&
      delaunayTriangulationOf [ Point { Pt.id = 1, coords = (1.0, 1.0) },
                                Point { Pt.id = 2, coords = (1.0, 1.0) }
                              ] == Graph empty
    it "Returns a very simple when given three points" $
      (let
          a = Point { Pt.id = 1, coords = (2.0, 2.0) }
          b = Point { Pt.id = 2, coords = (1.0, 1.0) }
          c = Point { Pt.id = 3, coords = (3.0, 1.0) }
       in
          delaunayTriangulationOf [ a, b, c ]
          == Graph (fromList [ meshPoint a [b,c], -- Use of Sets (not Lists) means we don't have to get the order right.  Yay!
                               meshPoint b [a,c],
                               meshPoint c [a,b]
                             ])
      )
      


{-
main = do
  putStrLn "----------------  Begin Test  ----------------"
  delaunayIO
  putStrLn "Test suite not yet implemented"
-}
