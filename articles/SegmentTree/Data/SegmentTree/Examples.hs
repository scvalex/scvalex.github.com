-- | Example uses of 'SegmentTree's.

module Data.SegmentTree.Examples 
    ( -- * Sum Monoid
      intervalSum
      -- * Any Monoid
    , intervalAny
      -- * GCD Monoid
    , GCD(..), intervalGCD
      -- * String Monoid
    , intervalConcat
      -- * Unwords Monoid
    , Unwords(..), intervalUnwords
    ) where

import Data.SegmentTree

import Data.Monoid

-------------
-- Sum monoid
-------------

-- | Find the sum of the elements in the interval [l, u].
intervalSum :: SegmentTree (Sum Int) -> (Int, Int) -> Int
intervalSum t bds@(l, u) = getSum $ queryTree t bds

-------------
-- Any monoid
-------------

-- | Find out if any of the elements are True in the interval [l, u].
intervalAny :: SegmentTree Any -> (Int, Int) -> Bool
intervalAny t bds@(l, u) = getAny $ queryTree t bds

-------------
-- GCD monoid
-------------

newtype (Integral a) => GCD a = GCD { getGCD :: a }

instance (Integral a) => Monoid (GCD a) where
    mempty = GCD $ fromIntegral 1
    (GCD x) `mappend` (GCD y) = GCD $ gcd x y
    
-- | Find the greatest common divisor of the elements in the interval
-- [l, u].
intervalGCD :: SegmentTree (GCD Int) -> (Int, Int) -> Int
intervalGCD t bds@(l, u) = getGCD $ queryTree t bds

----------------
-- String monoid
----------------

-- | Concatenate the strings in the interval [l, u].
intervalConcat :: SegmentTree String -> (Int, Int) -> String
intervalConcat t bds@(l, u) = queryTree t bds

---------------
-- Unwords monoid
---------------

newtype Unwords = Unwords { getUnwords :: String }

instance Monoid Unwords where
    mempty = Unwords ""
    (Unwords "") `mappend` y = y
    x `mappend` (Unwords "") = x
    (Unwords x) `mappend` (Unwords y) = Unwords (x ++ " " ++ y)

-- | Unwords the words in the interval [l, u].
intervalUnwords :: SegmentTree Unwords -> (Int, Int) -> String
intervalUnwords t bds@(l, u) = getUnwords $ queryTree t bds
