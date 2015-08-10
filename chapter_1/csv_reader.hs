{-# LANGUAGE DataKinds #-}

module Main where

import Text.CSV

main :: IO ()
main = do
  let filename = "input.csv"
  input <- readFile filename

  let csv = parseCSV filename input
  either handleError doWork csv

  where
    handleError csv = putStrLn "error parsing"
    doWork csv = (print . findOldest . tail) csv

findOldest :: [Record] -> Record
findOldest [] = []
findOldest xs = foldl1 (\a x -> if age x > age a then x else a) xs

age :: Record -> Int
age [a, b] = read b
age _ = 0
