-- | This module contains the 'SegmentTree' data structure, its
-- constructor and the query function.
--
-- Example Usage:
--
-- @
--     import Data.Monoid
--     import Data.SegmentTree
--     ...
--     st = mkTree $ map Sum [0..10]
--     ...
--     queryTree st (0, 10) == Sum 55
--     queryTree st (5, 10) == Sum 45
--     queryTree st (0, 4)  == Sum 10
-- @

module Data.SegmentTree ( SegmentTree(..), mkTree, queryTree ) where

import Data.Monoid
import Text.Printf

data (Monoid a) => Tree a = Branch a (Tree a) (Tree a) | Leaf a

getCargo (Branch x _ _) = x
getCargo (Leaf x)       = x

-- | A 'SegmentTree' is a binary tree and the bounds of its
-- corresponding interval.
data (Monoid a) => SegmentTree a = SegmentTree (Tree a) (Int, Int)

instance (Monoid a) => Show (SegmentTree a) where
    show (SegmentTree t (l, u)) = unlines $ go t (l, u)
        where
          go (Branch _ lc rc) (l, u) = 
              let m = (u-l) `div` 2
                  (ls, rs) = (go lc (l, l+m), go rc (l+m+1, u))
                  (ls', rs') = (indentTree True ls, indentTree False rs)
                  ts = printf "[%d..%d]" l u
              in concat [[ts], ls', rs']
          go (Leaf _) (l, u) = [printf "[%d]" l]
          indentTree _ [] = []
          indentTree True [x] = [printf "|-- %s" x]
          indentTree False [x] = [printf "`-- %s" x]
          indentTree True (x:xs) = indentTree True [x] ++ map ("|     "++) xs
          indentTree False (x:xs) = indentTree False [x] ++ map ("      "++) xs

-- | Build the 'SegmentTree' for the given list. Time: O(n*log n)
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

-- | Query the 'SegmentTree' for the specified closed interval. Time:
-- O(log n)
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
