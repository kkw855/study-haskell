import Data.Char

double' :: Num a => a -> a
double' x = x + x

doubleTwice' :: Num a => a -> a
doubleTwice' x = double' (double' x)

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x : xs) = x + sum' xs

product' :: Num a => [a] -> a
product' [] = 1
product' (x : xs) = x * product' xs

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]

qsortReverse :: Ord a => [a] -> [a]
qsortReverse [] = []
qsortReverse (x : xs) = qsortReverse larger ++ [x] ++ qsortReverse smaller
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]

div10 :: Int
div10 = a `div` length xs
  where
    a = 10
    xs = [1, 2, 3, 4, 5] :: [Int]

add' :: Int -> Int -> Int
add' x y = x + y

seqn :: Monad m => [m a] -> m [a]
seqn [] = return []
seqn (act : acts) = do
  x <- act
  xs <- seqn acts
  return (x : xs)

bools :: [Bool]
bools = [True, False]

nums :: [[Int]]
nums = [[1, 2], [3, 4]]

add2' :: Int -> Int -> Int -> Int
add2' x y z = x + y + z

copy' :: a -> (a, a)
copy' x = (x, x)

apply :: (a -> b) -> a -> b
apply f x = f x

test :: [Char] -> Bool
test ('a' : _) = True
test _ = False

add3' :: Int -> Int -> Int
add3' = \x -> \y -> x + y

const' :: a -> b -> a
const' x = \_ -> x

odds :: Int -> [Int]
odds n = map (\x -> x * 2 + 1) [0 .. n -1]

sum2' :: [Int] -> Int
sum2' = foldl (+) 0

concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs]

firsts :: [(a, b)] -> [a]
firsts ps = [x | (x, _) <- ps]

length' :: [a] -> Int
length' xs = sum [1 | _ <- xs]

factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1, n]

primes :: Int -> [Int]
primes n = [x | x <- [2 .. n], prime x]

find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0 ..], x == x']

lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

