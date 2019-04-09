module Graphics.Graph
  (
    Graph ( Graph )
  , Graphics.Graph.size
  , Graphics.Graph.addPt
  )
  where

-- import Graphics.WingedEdge
import Graphics.MeshPoint
import Graphics.Point

import Data.Set

-- | A graph is a collection of points, each of which is associated with a collection of adjacent points, which adjacent
-- points determine edges from the first point.
data Graph = Graph (Set MeshPoint)
  deriving (Eq, Show)

{-
instance Show Graph where
  show Graph mps = "graph g {\n"
                   ++ map showMeshPoint $ elems mps
                   ++ "}"
    where showMeshPoint mp = "\t" ++ -- ack, now what?  Need a point NAME, which we don't have yet
-}

size :: Graph -> Int
size (Graph mps) = Data.Set.size mps

addPt :: Graph -> Point -> Graph
addPt (Graph mps) pt = Graph (insert (meshPoint pt []) mps)
