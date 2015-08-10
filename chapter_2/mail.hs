{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative
--import Data.Attoparsec
import Data.Attoparsec.Text
import Data.Char (isAlphaNum)

data Email = Email
  { user :: String
  , host :: String
  , domain :: String
  } deriving Show

email :: Parser Email
email = Email
  <$> (skipSpace *> identity) -- user
  <*> (char '@'  *> identity) -- host
  <*> (char '.'  *> identity) -- doman
  where identity = many' $ satisfy isAlphaNum

main :: IO ()
main = do
  let Right a = parseOnly email "nishant@gmail.com"
  print a
