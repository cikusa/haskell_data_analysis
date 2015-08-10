module Main where

import Network.HTTP
import Network.URI (parseURI)
import Text.XML.HXT.Core
import Text.HandsomeSoup
import Data.Maybe (fromJust)

-- requestURL = "http://www.virginia.edu/cgi-local/ldapweb"
requestURL = "http://en.wikipedia.org"

myRequest :: String -> Request String
myRequest query = Request
  { rqURI = fromJust $ parseURI requestURL
  , rqMethod = POST
  , rqHeaders = [ mkHeader HdrContentType "text/html"
                , mkHeader HdrContentLength $ show $ length body]
  , rqBody = body
  }
  where body = ""

main :: IO ()
main = do
  response <- simpleHTTP $ myRequest "poon"
  html <- getResponseBody response
  let doc = readString [withParseHTML yes, withWarnings no] html
  rows <- runX $ doc >>> css "td" //> getText
  putStrLn $ unlines rows
