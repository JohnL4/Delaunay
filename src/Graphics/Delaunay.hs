module Graphics.Delaunay
    ( delaunayIO
    , delTri
    ) where

import Graphics.Point
-- import Graphics.WingedEdge as WE
import Graphics.MeshPoint as MP
import Graphics.Graph

import Data.Set

type Coords = ( Double, Double )

delaunayIO :: IO ()
delaunayIO = putStrLn "delaunayIO"

-- | Takes a list of points and returns a list of MeshPoints.
delTri :: [ Coords ] -> Graph
delTri []    = Graph empty
delTri [_]   = Graph empty
delTri [a,b] = Graph empty
{-
          a
         / \
        /   \
       b-----c
-}
delTri [aCoords, bCoords, cCoords]
  = Graph (fromList [ meshPoint a [ b, c ] 
                   , meshPoint b [ a, c ] 
                   , meshPoint c [ a, b ] 
                   ])
  where a = Point 0 aCoords
        b = Point 1 bCoords
        c = Point 2 cCoords

delTri (a:b:c:coordPairs) = addPts (delTri [a,b,c]) (identifiedPts coordPairs [3..])
  where identifiedPts [] _                          = []
        identifiedPts (coordPair:coordPairs) (n:ns) = (Point n coordPair) : (identifiedPts coordPairs ns)
        addPts g []                                 = g
        addPts g (pt:pts)                           = addPts (addPt g pt) pts
