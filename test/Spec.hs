import Test.Hspec

import Graphics.Delaunay
import Graphics.Point as Pt

main :: IO ()
main = hspec $ do
  describe "Basics/throat-clearing" $ do
    it "Processes an empty list of points" $
      delaunayTriangulationOf [] == []
    it "Returns empty set if there are fewer than three points" $
      delaunayTriangulationOf [ Point { Pt.id = 1, coords = (1.0, 1.0) } ] == []
      &&
      delaunayTriangulationOf [ Point { Pt.id = 1, coords = (1.0, 1.0) },
                                Point { Pt.id = 1, coords = (1.0, 1.0) }
                              ] == []
      


{-
main = do
  putStrLn "----------------  Begin Test  ----------------"
  delaunayIO
  putStrLn "Test suite not yet implemented"
-}
