module Main where

import Text.XML.HXT.Core
import Data.List.Split (chunksOf)
import Data.List

main :: IO ()
main = do
  input <- readFile "input.html"
  texts <- runX $ readString [withParseHTML yes, withWarnings no] input
    //> hasName "td"
    //> getText
  let rows = chunksOf 3 texts
  print $ findBiggest rows

findBiggest :: [[String]] -> [String]
findBiggest [] = []
findBiggest items = foldl1' func items
  where func a x = if capacity x > capacity a then x else a
        capacity [_, _, c] = read c :: Int
        capacity _ = -1
