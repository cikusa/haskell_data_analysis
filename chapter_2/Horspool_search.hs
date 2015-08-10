module Main where

import qualified Data.HashMap.Strict as M
import Data.Vector ((!))
import qualified Data.Vector as V
import Data.Hashable

import Debug.Trace

bmh :: (Eq a, Hashable a) => [a] -> [a] -> Maybe Int
bmh pat xs = bmh' begin begin 0
  where
    vxs    = V.fromList xs
    xs_len = V.length vxs
    vpat   = V.fromList pat
    begin  = V.length vpat - 1
    pmap   = revIndexMap vpat

    bmh' i j n
      | j < 0 = Just n
      | i >= xs_len = Nothing
      | vxs ! i == vpat ! j = bmh' (i - 1) (j - 1) n
      | otherwise = bmh' next_i next_j next_n
        where
          next_i = next_n + next_j
          next_j = begin
          next_n = maybe (n + j + 1) shift $ M.lookup (vxs ! i) pmap
          shift q = n + if q > j then 1 else i - q

revIndexMap :: (Eq a, Hashable a) => V.Vector a -> M.HashMap a Int
revIndexMap xs = M.fromList $ zip (reverse $ V.toList xs) [0..]
