main :: IO ()
main = do
  contents <- readFile "day2.txt"
  print (sum (filter isRepeating' (concatMap (idsToInts . splitIds) (splitRanges contents))))

splitRanges :: String -> [String]
splitRanges s = case break (== ',') s of
  (a, ',' : b) -> a : splitRanges b
  -- Hacky-ish way to get rid of newline
  (a, "") -> [reverse (drop 1 (reverse a))]

splitIds :: String -> (String, String)
splitIds s = case break (== '-') s of
  (a, '-' : b) -> (a, b)

idsToInts :: (String, String) -> [Int]
idsToInts (s, s') = [read s :: Int .. read s' :: Int]

isRepeating :: Int -> Bool
isRepeating i = i' == concat (replicate 2 (take (length i' `div` 2) i'))
  where
    i' = show i

isRepeating' :: Int -> Bool
isRepeating' i = foldr ((||) . (i' ==)) False ([concat (replicate (length i' `div` n) (take n i')) | n <- [1 .. (length i' `div` 2)]])
  where
    i' = show i
