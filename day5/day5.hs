main :: IO ()
main = do
  contents <- readFile "day5.txt"
  let ls = lines contents
  print (length (filter id (map (inRanges (map range (takeWhile (/= "") ls))) (map read (tail (dropWhile (/= "") ls)) :: [Int]))))

range :: String -> (Int, Int)
range s = case break (== '-') s of
  (a, '-' : b) -> (read a :: Int, read b :: Int)

inRanges :: [(Int, Int)] -> Int -> Bool
inRanges is n = foldr (||) False (map (inRange n) is)

inRange :: Int -> (Int, Int) -> Bool
inRange n is = n >= fst is && n <= snd is
