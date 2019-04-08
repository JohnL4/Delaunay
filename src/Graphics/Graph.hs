module Graphics.Graph
  (
    Graph ( Graph )
  )
  where

-- import Graphics.WingedEdge
import Graphics.MeshPoint
import Data.Set

-- | A graph is a collection of points, each of which is associated with a collection of adjacent points, which adjacent
-- points determine edges from the first point.
data Graph = Graph (Set MeshPoint)
  deriving (Eq, Show)
