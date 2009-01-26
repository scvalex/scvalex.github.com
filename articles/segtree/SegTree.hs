module SegTree ( SegmentTree(..), mkTree, queryTree ) where

import Data.Monoid

-- | Standard definition of a tree with unspecified cargo type.
data (Monoid a) => Tree a = Branch a (Tree a) (Tree a) | Leaf a

getCargo (Branch x _ _) = x
getCargo (Leaf x)       = x

-- | A Tree and the bounds of its corresponding interval.
data (Monoid a) => SegmentTree a = SegmentTree (Tree a) (Int, Int)

mkTree :: (Monoid a) => [a] -> SegmentTree a
mkTree xs = SegmentTree (go xs listBounds) listBounds
    where
      listBounds = (0, length xs - 1)
      go ys (l, u)
          -- invariant: head ys == xs !! l
          | l == u    = Leaf (head ys)
          | otherwise = 
              let m      = (u-l) `div` 2
                  leftc  = go ys (l, l+m)
                  rightc = go (drop (m+1) ys) (l+m+1, u)
              in Branch (getCargo leftc `mappend` getCargo rightc) 
                        leftc rightc

queryTree :: (Monoid a) => SegmentTree a -> (Int, Int) -> a
queryTree (SegmentTree t (s, e)) (l, u) = go t (s, e)
    where
      -- we're querying for (l, u)
      go t (s, e)
          | (l > e) || (u < s)   = mempty
          | (l <= s) && (u >= e) = getCargo t
          | otherwise = let (Branch _ leftc rightc) = t
                            m = (e-s) `div` 2
                            lv = go leftc (s, s+m)
                            rv = go rightc (s+m+1, e)
                        in lv `mappend` rv
