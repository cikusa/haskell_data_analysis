module Main where

import Data.List (elemIndices, foldl')

jaro :: Eq a => [a] -> [a] -> Double
jaro s1 s2
  | m == 0    = 0.0
  | otherwise = (1/3) * (m/ls1 + m/ls2 + (m-t)/m)
  where
    ls1 = fromIntegral $ length s1
    ls2 = fromIntegral $ length s2

    m' = matching s1 s2 d
    m = fromIntegral m'

    d = truncate $ max ls1 ls2 / 2 - 1
    t = fromIntegral $ (m' - matching s1 s2 0) `div` 2

matching :: Eq a => [a] -> [a] -> Int -> Int
matching s1 s2 d = foldl' match 0 $ zip s1 [0..]
  where
    match acc (c, i)
      | any (<= d) $ map (abs . (i-)) (elemIndices c s2) = acc + 1
      | otherwise = acc
