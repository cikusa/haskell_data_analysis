module Main where

import Control.Monad
import Control.Applicative
import Data.HashMap.Strict (HashMap)
import Data.List
import Data.Maybe (fromJust)

import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S

count :: [Int] -> HashMap Int Int
count = foldl' (\m n -> M.insertWith (+) n 1 m) M.empty

removeDup :: [Int] -> [Int]
removeDup = reverse . snd . foldl' go (S.empty, [])
  where go (s, xs) x = if S.member x s then (s, xs) else (S.insert x s, x:xs)

main :: IO ()
main = do
  t <- readLn
  replicateM_ t $ do
    [_, k] <- getLineNums
    ns <- getLineNums

    let frq = count ns
    let res = [x | x <- removeDup ns, M.lookupDefault 0 x frq >= k]

    if null res then print (-1 :: Int) else putStrLn $ unwords $ map show res

  where
    getLineNums = (map readInt . BS.words) <$> BS.getLine
    readInt = fst . fromJust . BS.readInt
