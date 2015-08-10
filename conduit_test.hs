module Main where

import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB

main :: IO ()
main = do
  result <- CL.sourceList [1..10 :: Int] $$ CL.fold (+) 0
  print result

  result <- CL.sourceList [1..10 :: Int] $$ CL.map (+1) =$ CL.consume
