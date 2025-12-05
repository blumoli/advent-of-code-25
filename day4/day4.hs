main :: IO ()
main = do
  contents <- readFile "day4test.txt"
  let ls = lines contents
  print ls

-- break (=='@') (head ["..@@.@@@@.","@@@.@.@.@@","@@@@@.@.@@","@.@@@@..@.","@@.@@@@.@@",".@@@@@@@.@",".@.@.@.@@@","@.@@@.@@@@",".@@@@@@@@.","@.@.@@@.@."])

kernel :: [String] -> [String]
kernel image = undefined
