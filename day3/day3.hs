import Data.Char (digitToInt)

main :: IO ()
main = do
  contents <- readFile "day3.txt"
  let ls = lines contents
  print (sum (map (joulesPairToInt . findMaxJoules) (stringToDigits ls)))

stringToDigits :: [String] -> [[Int]]
stringToDigits = map (map digitToInt)

findMaxJoules :: [Int] -> (Int, Int)
findMaxJoules is =
  if maximum is == last is
    then (maximum (init is), maximum is)
    else (maximum is, maximum (tail (dropWhile (/= maximum is) is)))

joulesPairToInt :: (Int, Int) -> Int
joulesPairToInt p = fst p * 10 + snd p
