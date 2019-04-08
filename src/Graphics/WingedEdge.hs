-- Maybe a winged edge will be useful someday, but not today.

module Graphics.WingedEdge
  (
    WingedEdge ( WingedEdge ),
    Graphics.WingedEdge.id, startPt, endPt, ccwNext, ccwPrev, cwNext, cwPrev
  )
  where

import Graphics.Point

-- | A unique id
type Id = Integer

-- | A "winged edge"; an edge with next-neighbors
data WingedEdge = WingedEdge { id :: Id,            -- ^ Unique id for this edge
                               startPt :: Point,    -- ^ Where the edge starts
                               endPt :: Point,      -- ^ Where the edge ends
                               ccwNext :: Maybe Id, -- ^ Id of next edge when traversing in a counterclockwise direction
                               ccwPrev :: Maybe Id, -- ^ Id of previous edge when traversing in a counterclockwise direction
                               cwNext :: Maybe Id,  -- ^ Id of next edge when traversing in a clockwise direction
                               cwPrev :: Maybe Id   -- ^ Id of previous edge when traversing in a clockwise direction
                             }


instance Eq WingedEdge where
  a == b = startPt a == startPt b && endPt a == endPt b
