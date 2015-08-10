module Main where

  import Text.XML.HXT.Core
  import Text.HandsomeSoup

  main :: IO ()
  main = do
    let doc = fromUrl "http://www.csdn.net/"
    links <- runX $ doc >>> css "#bodyContent a" ! "href"
    print links
