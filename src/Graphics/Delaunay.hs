module Graphics.Delaunay
    ( delaunayIO
    , delaunayTriangulationOf
    ) where

import Graphics.Point
import Graphics.WingedEdge as WE

delaunayIO :: IO ()
delaunayIO = putStrLn "delaunayIO"

-- | Takes a list of points (TODO: define) and returns a list of WingedEdges (TODO: define)
delaunayTriangulationOf :: [ Point ] -> [ WingedEdge ]
delaunayTriangulationOf []    = []
delaunayTriangulationOf [_]   = []
delaunayTriangulationOf [a,b] = []
{-
  = [ WingedEdge { WE.id = 1,
                   startPt = a,
                   endPt = b,
                   ccwNext = Nothing,
                   ccwPrev = Nothing,
                   cwNext = Nothing,
                   cwPrev = Nothing }
    ]
-}
delaunayTriangulationOf [a,b,c]
{-
          a
         / \
        /   \
       b-----c
-}
  = [ WingedEdge { WE.id = 1,
                   startPt = a,
                   endPt = b
                   -- ccwNext = 2,  -- b
                   -- ccwPrev = 3,  -- c
                   -- cwNext = 3,   -- c
                   -- cwPrev = 2    -- b
                 },
      WingedEdge { WE.id = 2,
                   startPt = b,
                   endPt = c    -- this is stupid.  It's not Delaunay's job to figure out clockwise-ness.
                 },
      WingedEdge { WE.id = 3,
                   startPt = c,
                   endPt = a
                 }
    ]
