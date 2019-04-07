module Graphics.Graph
  (
    Graph
  )
  where

import Graphics.WingedEdge

-- | A graph is a list of winged edges.  There are no isolated points, since we're just interested in drawing lines, not
-- dots.
data Graph = Graph [ WingedEdge ]
