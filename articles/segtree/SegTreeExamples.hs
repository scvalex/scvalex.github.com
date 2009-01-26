module SegTreeExamples where

import SegTree

import Data.Monoid

-------------
-- Sum monoid
-------------

intervalSumTree :: SegmentTree (Sum Int)
intervalSumTree = mkTree $ map Sum [0..100]

-- Find the sum of the elements in the interval [l, u].
intervalSum :: SegmentTree (Sum Int) -> (Int, Int) -> Int
intervalSum t bds@(l, u) = getSum $ queryTree t bds

-------------
-- Any monoid
-------------

intervalAnyTrueTree :: SegmentTree Any
intervalAnyTrueTree = mkTree $ map Any $ take 100 
                             $ iterate not False

-- Find out if any of the elements are True in the interval [l, u].
intervalAny :: SegmentTree Any -> (Int, Int) -> Bool
intervalAny t bds@(l, u) = getAny $ queryTree t bds

-------------
-- GCD monoid
-------------

newtype (Integral a) => GCD a = GCD { getGCD :: a}

instance (Integral a) => Monoid (GCD a) where
    mempty = GCD $ fromIntegral 1
    (GCD x) `mappend` (GCD y) = GCD $ gcd x y
    
intervalGCDTree :: SegmentTree (GCD Int)
intervalGCDTree = mkTree $ map GCD [0, 2..200]

-- Find the greatest common divisor of the elements in the interval
-- [l, u].
intervalGCD :: SegmentTree (GCD Int) -> (Int, Int) -> Int
intervalGCD t bds@(l, u) = getGCD $ queryTree t bds

----------------
-- String monoid
----------------

intervalStringTree :: SegmentTree String
intervalStringTree = mkTree [['a'..u] | u <- ['a'..'z']]

-- Concatenate the strings in the interval [l, u].
intervalConcat :: SegmentTree String -> (Int, Int) -> String
intervalConcat t bds@(l, u) = queryTree t bds
