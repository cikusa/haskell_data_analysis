module Main where

import Text.XML.HXT.Core

main :: IO ()
main = do
  input <- readFile "input.xml"
  dates <- runX $ readString [withValidate no] input
    //> hasName "date"
    //> getText
  print dates
