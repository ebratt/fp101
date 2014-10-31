--Caeser's Cipher example...
isLower :: Char -> Bool
isLower x = x >= 'a' && x <= 'z'


lowers :: String -> Int
lowers xs = length (filter (isLower) xs)

count :: Char -> String -> Int
count x xs = length (filter (\x' -> x == x') xs)

ord :: Char -> Int
ord c = snd ((\(x:xs) -> x) ((filter (\(k,v) -> k == c) (zip ['a'..'z'][0..25]))))

chr :: Int -> Char
chr n = snd ((\(x:xs) -> x) ((filter (\(k,v) -> k == n) (zip [0..25]['a'..'z']))))

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2let((let2int c+n) `mod` 26)
  | otherwise = c

encode :: Int -> String -> String
encode n xs = map (\x -> shift n x) xs

table :: [Float]
table = [8.2, 1.5, 2.8, 4.0, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 
         6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent(count x xs) n | x <- ['a'..'z']]
  where n = lowers xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e)^2) / e | (o, e) <- (zip os es)]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i|(x',i) <- zip xs [0..n], x == x']
  where n = length xs - 1

crack :: String -> String
crack xs = encode (-factor) xs
  where
    factor = head(positions(minimum chitab) chitab)
    chitab = [chisqr(rotate n table') table | n <- [0..25]]
    table' = freqs xs

--pyths :: Int -> [(Int, Int, Int)]
--pyths n = filter (x^2 + y^2 == z^2) [1..n]


-- Recursion
factorial :: Integer -> Integer
--factorial n = product[1..n]
factorial 0     = 1
factorial n
  | n > 0     = n * (factorial (n - 1))
  | otherwise = error "cannot have a negative number!"

-- multiplication can be reduced to repeated addition...
(*!) :: Int -> Int -> Int
m *! 0 = 0
m *! n = m + (m *! (n - 1))

product2 :: Num a => [a] -> a
product2 []     = 1
product2 (n:ns) = n * product2 ns

length2 :: [a] -> Int
length2 []     = 0
length2 (_:xs) = 1 + length2 xs

reverse2 :: [a] -> [a]
reverse2 []     = []
reverse2 (x:xs) = reverse2 xs ++ [x]

insert :: Ord a => a -> [a] -> [a]
insert x []   = [x]
insert x (y:ys) 
  | x <= y    = x:y:ys
  | otherwise = y:insert x ys

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 2) + fibonacci (n - 1)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a|a <- xs, a <= x]
    larger = [b|b <- xs, b > x]

odds :: [a] -> [a]
odds [] = []
odds (_:xs) = evens xs

evens :: [a] -> [a]
evens [] = []
evens (x:xs) = x : odds xs

product3 :: Num a => [a] -> a
product3 = foldr (*) 1

drop2 :: Int -> [a] -> [a]
drop2 0 xs     = xs
drop2 n []     = []
drop2 n (_:xs) = drop (n-1) xs

init2 :: [a] -> [a]
init2 []     = []
init2 (_:[]) = []
init2 (x:xs) = x : init2 xs

