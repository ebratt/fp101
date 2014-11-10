--week 3
--conditionals are written with if..then..else
--abs2 :: Int -> Int
--abs2 n = if n >= 0 then n else (-n)

--as an alternative, you can use "guarded equations"
abs2               :: Int -> Int
abs2 n | n >= 0    = n
       | otherwise = -n

signum2              :: Int -> Int
signum2 n | n < 0     = -1
          | n == 0    = 0
          | otherwise = 1

--lambda (anonymous) functions allow you to curry and reason about functions
odds :: [Integer] -> [Integer]
odds [] = []
odds xs = map (\x -> x*2 + 1) xs

-- list comprehensions
squares :: [Int] -> [Int]
squares xs = map (\x -> x*x) xs

factors :: Int -> [Int]
factors n = filter (\x -> n `mod` x == 0) [1..n]
    --[x | x <- [1..n], n `mod` x == 0]

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = 
    and [x <= y | (x,y) <- pairs xs]

--positions :: Eq a => a -> [a] -> [Int]
--positions x xs = 
--    [i | (x', i) <- zip xs [0..n], x == x']
--    where n = length xs - 1

isLower :: Char -> Bool
isLower a = a <= 'z' && a >= 'a'

lowers :: String -> Int
lowers xs = 
    length [x | x <- xs, isLower x]


-- recursive functions
-- tail-call elimination is very important; typically you don't have this in imperative languages
factorial    :: Int -> Int
-- non-recursive definition
--factorial n = product [1..n] 
--recursive definition
factorial 0   = 1
factorial n 
  | n > 0     = n * factorial (n-1)
  | otherwise = error "cannot calculate factorial of a negative number"

product'        :: [Int] -> Int
product' []     = 1
product' (n:ns) = n * product' ns

length'        :: [a] -> Int
length' []     = 0
length' (_:xs) = 1 + length' xs

reverse'        :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

zip'               :: [a] -> [b] -> [(a,b)]
zip' [] _          = []
zip' _ []          = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (_:xs) = drop' (n-1) xs

{-(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)
-}

--quicksort in recursive definition
quicksort        :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (x:xs) = quicksort smaller ++ [x] ++ quicksort larger
  where smaller  = [a | a <- xs, a <= x] -- reads, "a list of a's such that each a is <= x"
        larger   = [b | b <- xs, b > x] -- reads, "a list of b's such that each b is > x"

rep     :: Int -> a -> [a]
rep 0 _ = []
rep n x = x : rep (n-1) x

select          :: [a] -> Int -> a
select [] _     = error "cannot select from an empty list"
select (x:xs) 0 = x
select (x:xs) n = select xs (n-1)

exists          :: Eq a => a -> [a] -> Bool
exists _ []     = False
exists x (y:ys) = if x == y then True else exists x ys

