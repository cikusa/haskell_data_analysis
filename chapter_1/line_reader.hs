module Main where

import Control.Exception (catch, SomeException)
import System.Environment (getArgs)

countWords :: String -> [Int]
countWords = map (length . words) . lines

main :: IO ()
main = do
  file_name <- getArgs >>= \args ->
    case args of
      (a:_)     -> return a
      otherwise -> return "input.txt"
  input <- catch (readFile file_name)
    $ \err -> print (err::SomeException) >> return ""
  print $ countWords input
