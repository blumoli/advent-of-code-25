main = do
  contents <- readFile "day2test.txt"
  print $ concatMap (idsToInts . splitIds) (splitRanges contents)

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

isRepeating :: Int -> Int -> Bool
isRepeating i l = show i == undefined
