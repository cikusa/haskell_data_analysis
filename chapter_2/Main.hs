module Main where

import Data.char (isSpace)

trim :: String -> String
trim = unwords . words

punctuations :: [Char]
punctuations = ['!', '"', '#', '$', '%'
               , '(', ')', '.', ',', '?']
removePunctuations :: String -> String
removePunctuations = filter (`notElem` punctuations)

specialSymbols :: [Char]
specialSymbols = ['/', '-']

replaceSpecialSymbols :: String -> String
replaceSpecialSymbols = map $ \x -> if c `elem`specialSymbols then ' ' else c
