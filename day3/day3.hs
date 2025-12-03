import Data.Char (digitToInt)

main :: IO ()
main = do
  contents <- readFile "day3.txt"
  let ls = lines contents
  print (sum (map (joulesListToInt 11 . findMaxJoules' 11) (stringToDigits ls)))

-- print (sum (map (joulesPairToInt . findMaxJoules) (stringToDigits ls)))

stringToDigits :: [String] -> [[Int]]
stringToDigits = map (map digitToInt)

findMaxJoules :: [Int] -> (Int, Int)
findMaxJoules is =
  if maximum is == last is
    then (maximum (init is), maximum is)
    else (maximum is, maximum (tail (dropWhile (/= maximum is) is)))

-- try taking from only first n elements for each recursion call
findMaxJoules' :: Int -> [Int] -> [Int]
findMaxJoules' 0 js = [maximum js]
findMaxJoules' o js = j' : findMaxJoules' (o - 1) (tail (dropWhile (/= j') js))
  where
    j' = maximum (take (length js - o) js)

joulesPairToInt :: (Int, Int) -> Int
joulesPairToInt p = fst p * 10 + snd p

joulesListToInt :: Int -> [Int] -> Int
joulesListToInt d [] = 0
joulesListToInt d (j : js) = j * 10 ^ d + joulesListToInt (d - 1) js
