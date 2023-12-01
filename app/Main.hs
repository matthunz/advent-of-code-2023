module Main where

import Data.Char (digitToInt, isDigit)
import System.IO (IOMode (ReadMode), hGetContents, openFile)

main :: IO ()
main = do
  n <- run "1" solve1B
  print n

run :: String -> (String -> a) -> IO a
run name f = do
  handle <- openFile ("input/" ++ name) ReadMode
  contents <- hGetContents handle
  pure $ f contents

calibrationValue :: [Int] -> Int
calibrationValue digits = head digits * 10 + last digits

solve1 :: (String -> [Int]) -> String -> Int
solve1 f s = sum $ map (calibrationValue . f) $ lines s

solve1A :: String -> Int
solve1A s = solve1 f s
  where
    f s = map digitToInt $ filter isDigit s

digitFromChars :: String -> Maybe Int
digitFromChars "" = Nothing
digitFromChars s = case f s of
  Just n -> Just n
  Nothing -> digitFromChars (tail s)
  where
    f s = case s of
      "one" -> Just 1
      "two" -> Just 2
      "three" -> Just 3
      "four" -> Just 4
      "five" -> Just 5
      "six" -> Just 6
      "seven" -> Just 7
      "eight" -> Just 8
      "nine" -> Just 9
      _ -> Nothing

solve1B :: String -> Int
solve1B = solve1 f
  where
    f s =
      reverse $
        snd $
          foldr
            ( \x (acc, vals) ->
                ( if isDigit x
                    then ("", digitToInt x : vals)
                    else case digitFromChars (acc ++ [x]) of
                      Just n -> ([x], n : vals)
                      Nothing -> (acc ++ [x], vals)
                )
            )
            ("", [])
            (reverse s)
