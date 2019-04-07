module Graphics.Point
  (
    Point (Point),
    Graphics.Point.id, coords
  )
  where

-- | A unique id
type Id = Integer

-- | A 2-D point.
data Point = Point { id :: Integer,               -- ^ Unique id for this point
                     coords :: (Double, Double) } -- ^ (x,y) coordinates for point

instance Eq Point where
  a == b = coords a == coords b
