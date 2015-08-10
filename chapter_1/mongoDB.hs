{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Main where

import Database.MongoDB

main :: IO ()
main = do
  pipe <- connect (host "127.0.0.1")
  e <- access pipe master "test" run
  close pipe
  print e

run = getData
getData = rest =<< find (select [] "people") {sort = []}
