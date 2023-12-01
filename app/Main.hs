module Main where

import Data.Char (digitToInt, isDigit)
import System.IO ( hGetContents, openFile, IOMode(ReadMode) )

main :: IO ()
main = do
  n <- run "1" solve1A
  print n

run :: String -> (String -> a) -> IO a
run name f = do
  handle <- openFile ("input/" ++ name) ReadMode
  contents <- hGetContents handle
  pure $ f contents

solve1A :: String -> Int
solve1A s = sum $ map calibrationValue $ lines s

calibrationValue :: String -> Int
calibrationValue s = head digits * 10 + last digits
  where
    digits = map digitToInt $ filter isDigit s
