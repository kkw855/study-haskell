double' :: Num a => a -> a
double' x = x + x

doubleTwice' :: Num a => a -> a
doubleTwice' x = double' (double' x)

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x : xs) = x + sum' xs

-- product' [2,3,4] “ 24
product' :: Num a => [a] -> a
product' [] = 1
product' (x : xs) = x * product' xs

-- qsort [3,5,1,4,2] = [1,2,3,4,5]
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]

-- qsortReverse [3,5,1,4,2] = [5,4,3,2,1]
qsortReverse :: Ord a => [a] -> [a]
qsortReverse [] = []
qsortReverse (x : xs) = qsortReverse larger ++ [x] ++ qsortReverse smaller
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]

n :: Int
n = a `div` length xs
  where
      a = 10
      xs = [1,2,3,4,5] :: [Int]

add' :: Int -> Int -> Int
add' x y = x + y