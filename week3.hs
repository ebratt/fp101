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

--start of homework 3
--invalid
{-halve1 xs = (take n xs, drop n xs)
  where n = length xs / 2-}

--valid
halve2 xs = splitAt (length xs `div` 2) xs

--valid
halve3 xs = (take (n `div` 2) xs, drop (n `div` 2) xs)
  where n = length xs

--invalid 
--halve4 xs = splitAt (length xs `div` 2)

--valid
halve5 xs = (take n xs, drop (n + 1) xs)
  where n = length xs `div` 2

--valid
halve6 xs = splitAt (div (length xs) 2) xs

--invalid
--halve7 xs = splitAt (length xs / 2) xs

halve8 xs = (take n xs, drop n xs)
  where n = length xs `div` 2

