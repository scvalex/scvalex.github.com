import SegTree

import Data.Monoid
import Text.Printf ( printf )

import Test.QuickCheck
import Test.QuickCheck.Test hiding ( test )

reformList t@(SegmentTree _ (s, e)) = map (\i -> queryTree t (i, i)) [s..e]

prop_MkUnMkTree :: [Int] -> Bool
prop_MkUnMkTree xs = let mxs = map Sum xs
                         t = mkTree mxs
                         rxs = reformList t
                     in mxs == rxs

stupidSum :: [Int] -> (Int, Int) -> Int
stupidSum xs (l, u) = sum . take (u-l+1) . drop l $ xs

prop_SumOk :: [Int] -> Bool
prop_SumOk xs = let mxs = map Sum xs
                    t = mkTree mxs
                in all id [stupidSum xs (l, u) == getSum (queryTree t (l, u))
                          | l <- [0..length xs - 1]
                          , u <- [l..length xs - 1]]

prop_InvertedIntervalSum :: [Int] -> Bool
prop_InvertedIntervalSum xs = let mxs = map Sum xs
                                  t = mkTree mxs
                              in all id [0 == getSum (queryTree t (l, u))
                                        | l <- [0..length xs - 1]
                                        , u <- [0..l-1]]

props = [ ("mkTree reversible", quickCheck prop_MkUnMkTree)
        , ("sum ok", quickCheck prop_SumOk)
        , ("inverted intervals", quickCheck prop_InvertedIntervalSum)]

main = do
  putStrLn "Testing SegTree"
  mapM_ (\(s, a) -> printf "  %-30s: " s >> a) props
  putStrLn "All tests done"
