-- Chapter 3 함수의 구문
-- 패턴 매칭
lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky _ = "Sorry, you're out of lucky, pal!"

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors a b = (fst a + fst b, snd a + snd b)

addVectors' :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

head' :: [a] -> a
head' []      = error "Can't call head on an empty list, dummy!"
head' (x:_)   = x

-- as 패턴
firstLetter :: String -> String
firstLetter []              = "Empty string, whoops!"
firstLetter _all@(x:_)      = "The first letter of " ++ _all ++ " is " ++ [x]

-- 가드
bmiTell :: Double -> String
bmiTell bmi
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise   = "You're a whale, congratulations!"

-- where
bmiTell' :: Double -> Double -> String
bmiTell' weight height
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise   = "You're a whale, congratulations!"
    where bmi = weight / height ** 2

-- where 범위
greet :: String -> String
greet "Juan"        = niceGreeting ++ " Juan!"
    where niceGreeting = "Hello! So very nice to see you,"
greet "Ferando"     = niceGreeting ++ " Ferando!"
    where niceGreeting = "Hello! So very nice to see you,"
greet name          = badGreeting ++ " " ++ name
    where badGreeting  = "Oh! Pfft. It's you."

-- let 표현식
cylinder :: Double -> Double -> Double
cylinder r h =
    let sideArea  = 2 * pi * r * h
        topArea   = pi * r ** 2
    in sideArea + 2 * topArea

-- Chapter 4 재귀
maximum' :: (Ord a) => [a] -> a
maximum' []     = error "maximum of empty list!"
maximum' [x]    = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: Int -> a -> [a]
replicate' n a
    | n <= 0    = []
    | otherwise = a : replicate' (n-1) a

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0    = []
take' _ []      = []
take' n (x:xs)  = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' a = a : repeat' a

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _             = []
zip' _ []             = []
zip' (x:xs) (y:ys)    = (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ []        = False
elem' a (x:xs)
    | a == x      = True
    | otherwise   = a `elem'` xs

-- Chapter 5 고차원 함수
-- 커리된 함수, 부분 애플리케이션(부분적으로 적용된 함수)
compareWithHundred :: Int -> Ordering
compareWithHundred x = compare 100 x

compareWithHundred' :: Int -> Ordering
compareWithHundred' = compare 100

-- 중위 함수에 섹션
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphabet :: Char -> Bool
isUpperAlphabet = (`elem` ['A'..'Z'])

-- 함수를 받는 함수
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _           = []
zipWith' _ _ []           = []
zipWith' f (x:xs) (y:ys)  = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y

map' :: (a -> b) -> [a] -> [b]
map' _ []         = []
map' f (x:xs)     = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ []      = []
filter' p (x:xs)
    | p(x)        = x : filter' p xs
    | otherwise   = filter' p xs
