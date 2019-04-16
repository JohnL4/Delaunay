module TriTree
  (
    TriTree (TriTree, triangle, isOld, kids, parents, neighbors)
  , addPoint, flipEdge
  )
  where

{-
   This is some stupid tree/dag thing to go with the faniry-aims paper (see Fig. 4.2).

   Initial implementation will be completely naive.

   This is a tree of triangles.

   Each triangle has children:  the new triangles resulting from putting a point in the old triangle.

   Each triangle has parents:  the old triangles fractured to make this triangle.

   Each triangle has an "old" property:  initially false, flipped to true when the triangle is fractured by a new point
   or an edge flip.

   Each triangle has neighbors:  the three triangles neighboring this triangle.

   Operations are:

   addTriangles3 -- normal case, point in triangle.  New triangles are neighbors of each other, at least.  Furthermore,
   each new triangle may have a neighbor that is an existing triangle, from (old) parent's neighbors (max. 3).

   addTriangles2 -- edge case:  new point on edge of triangle.  New triangles are neighbors of each other.  Neighbors
   from parent's, as above.

   addTriangles2WithAdd'lParent -- edge flip.  New triangles are neighbors of each other.  Also, may have parents from
   set of both old parents' neighbors (max. 4 candidates to be neighbors, since conjoined parents have four edges).

-}

import Data.Set as Set
import Graphics.Point

-- data TreeDag a = EmptyTree | Node a (Set (TreeDag a))

-- | A dumb daggy tree.
data TriTree = TriTree {
  triangle :: Set Point,   -- ^ Exactly 3 points -- We use sets so we can use intersections to see if two triangles are
                           -- neighbors.
  isOld :: Bool,           -- ^ Whether or not this triangle is old.
  kids :: Set TriTree,     -- ^ 0, 2, or 3 kids
  parents :: Set TriTree,  -- ^ 1+ parents (hmm, this node should be in parent's set of kids) -- do we need a zipper of
                           -- some sort?
  neighbors :: Set TriTree -- ^ 0-3 neighbors (is it possible for a triangle to have one neighbor?).  Note that it is
                           -- not necessarily the case that a triangle's neighbors are its siblings in the tree
                           -- structure.
  }
  deriving (Eq, Ord, Show)

-- | Returns two or three triangles, each having as parent the given old triangle, and neighbors from the set of old
-- triangle's neighbors.
addPoint :: TriTree -> Point -> Set TriTree
addPoint _ _ = error "not implemented"

-- | Flips the edge between two triangles, return two different triangles, with new parents and neighbors.  Old
-- triangles get marked "old".
flipEdge :: TriTree -> TriTree -> (TriTree, TriTree)
flipEdge _ _ = error "not implemented"



{-
data TreeDag a = TreeDag {
  node :: a,                     -- ^ The node's contents.
  kids :: Set (TreeDag a),   -- ^ The node's children.  This set may be empty, in which case this is a leaf node.
                                 -- Following these links is expected to always arrive at a leaf node.
  parents :: Set (TreeDag a) -- ^ The node's parents.  This set is expected to always have size 1, at least.
                                 -- Following a chain of parents is expected to always lead to the tree's root.
  }
-}
