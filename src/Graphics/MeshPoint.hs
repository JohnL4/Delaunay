module Graphics.MeshPoint
  (
    MeshPoint ( MeshPoint )
  , point, adj
  , meshPoint
  )
  where

import Data.Set

import Graphics.Point

-- | A point in a (triangular, we hope) mesh.
data MeshPoint = MeshPoint { point :: Point,  -- ^ The point itself
                             adj :: Set Point -- ^ The point's adjacent neighbors, all of which determine an edge from
                                              -- the point.
                           }
                 deriving (Eq, Show, Ord)

meshPoint :: Point -> [ Point ] -> MeshPoint
meshPoint p ns = MeshPoint { point = p, adj = fromList ns }
