data Direction = R | L
  deriving (Show)

type Rotation = (Direction, Int)

type Dial = Int

type Count = Int

rotateDial :: Count -> Dial -> [Rotation] -> Count
rotateDial c d (r : rs) = case r of
  (R, n) -> rotateDial (checkIfZero c d) (d + n - (revolutions d r * 100)) rs
  (L, n) -> rotateDial (checkIfZero c d) (d - n + (revolutions d r * 100)) rs
rotateDial c d [] = checkIfZero c d

revolutions :: Dial -> Rotation -> Int
revolutions d r = case r of
  (R, n) -> (d + n) `div` 100
  (L, n) -> abs ((d - n) `div` 100)

checkIfZero :: Count -> Dial -> Count
checkIfZero c d = if d == 0 then c + 1 else c

main = do
  contents <- readFile "day1.txt"
  let ls = lines contents
  print (rotateDial 0 50 (concatMap stringToRotation ls))

stringToRotation :: String -> [Rotation]
stringToRotation cs = case head cs of
  'R' -> replicate (read (tail cs) :: Int) (R, 1)
  'L' -> replicate (read (tail cs) :: Int) (L, 1)
  _ -> undefined
