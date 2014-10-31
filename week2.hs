l1 = [1..10]
l2 = ['a'..'j']

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

add :: (Int, Int) -> Int
add (x,y) = x+y

add' :: Int -> Int -> Int
add' x y = x+y

mult :: Int -> Int -> Int -> Int
mult x y z = x*y*z

odds :: Int -> [Int]
odds n = map (\x -> x * 2 + 1) [0..n-1]

halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve list
  | odd (length list) = error "must have even number of elements in the list"
  | otherwise = splitAt (div (length list) 2) list

safeTail :: [a] -> [a]
safeTail []     = []
safeTail (_:xs) = xs

test :: (Bool, Bool) -> Bool
test (a, b) = if (a == True) then b
    else False

factors :: Integer -> [Integer]
factors n = filter (\x -> n `mod` x == 0) [1..n]

prime :: Integer -> Bool
prime n = factors n == [1,n]

primes :: Integer -> [Integer]
primes n = filter (\x -> prime x) [2..n]

list = [('a',1), ('b',2), ('c',3), ('b', 4)]

find :: Eq a => a -> [(a,b)] -> [b]
find k t = map (\(_,v) -> v) (filter (\(k',v) -> k == k') t)

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

ps = pairs[1,2,3,4]

sortedPair :: Ord a => (a, a) -> Bool
sortedPair (x, y) = x <= y

sorted :: Ord a => [a] -> Bool
sorted xs = and (\(x,y) -> sortedPair(x,y)) (pairs xs)

positions :: Eq a => a -> [a] - [Int]
positions x xs = zip xs [0..n]