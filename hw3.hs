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

--valid
halve8 xs = (take n xs, drop n xs)
  where n = length xs `div` 2

--valid
safetail1 xs = if null xs then [] else tail xs

--valid
safetail2 [] = []
safetail2 (_ : xs) = xs

--invalid
safetail3 (_ : xs)
  | null xs = []
  | otherwise = tail xs

--valid
safetail4 xs
  | null xs = []
  | otherwise = tail xs

--invalid
--safetail5 xs = tail xs
--safetail5 [] = []

--valid
safetail6 [] = []
safetail6 xs = tail xs

--invalid
safetail7 [x] = [x]
safetail7 (_ : xs) = xs

--valid
safetail8
  = \ xs ->
      case xs of
          [] -> []
          (_ : xs) -> xs

--valid
or1 :: Bool -> Bool -> Bool
or1 False False = False
or1 _ _ = True

--valid
or2 :: Bool -> Bool -> Bool
or2 False b = b
or2 True _ = True

--invalid
or3 :: Bool -> Bool -> Bool
or3 b c
  | b == c = True
  | otherwise = False

--valid
or4 :: Bool -> Bool -> Bool
or4 b c
  | b == c = b
  | otherwise = True

--valid
or5 :: Bool -> Bool -> Bool
or5 b False = b
or5 _ True = True

--valid
or6 :: Bool -> Bool -> Bool
or6 b c
  | b == c = c
  | otherwise = True

--invalid
--or7 :: Bool -> Bool -> Bool
--or7 b True = b
--or7 _ True = True

--valid
or8 :: Bool -> Bool -> Bool
or8 False False = False
or8 False True = True
or8 True False = True
or8 True True = True

--valid
and1 :: Bool -> Bool -> Bool
and1 True True = True
and1 _ _ = False

--valid
and2 :: Bool -> Bool -> Bool
and2 a b = if a then if b then True else False else False

--invalid
and3 :: Bool -> Bool -> Bool
and3 a b = if not(a) then not(b) else True

--invalid
--and4 :: Bool -> Bool -> Bool
--and4 a b = if a then b

--invalid
and5 :: Bool -> Bool -> Bool
and5 a b = if a then if b then False else True else False

--valid
and6 :: Bool -> Bool -> Bool
and6 a b = if a then b else False

--valid
and7 :: Bool -> Bool -> Bool
and7 a b = if b then a else False

--invalid
mult1 x y z = \ x -> (\ y -> (\ z -> x * y * z))

--invalid
--mult2 = \ x -> (x * \ y -> (y * \ z -> z))

--valid
mult3 = \ x -> (\ y -> (\ z -> x * y * z))

--invalid
--mult4 = ((((\x -> \y) -> \z) -> x * y) * z)

--invalid
remove1 n xs = take n xs ++ drop n xs

--invalid
remove2 n xs = drop n xs ++ take n xs

--invalid
remove3 n xs = take (n + 1) xs ++ drop n xs

--valid
remove4 n xs = take n xs ++ drop (n + 1) xs

func :: Int -> [a] -> [a]
func x xs = take (x + 1) xs ++ drop x xs

--start of lab 
e0 = [False, True, False, True]

e1 :: Num t => [[t]]
e1 = [[1,2],[3,4]]

--e2 :: Eq t => [[[t]]]
--e2 = [[[1,2,3]],[[3,4,5]]]

e3 x = x * 2

e4 (x, y) = x

e5 (x, y, z) = z

e6 x y = x * y

e7 (x, y) = (y, x)

e8 x y = (y, x)

e9 [x,y] = (x, True)

e10 (x,y) = [x,y]

e11 :: (Char, Bool)
e11 = ('\a', True)

e12 :: [(Char, Int)]
e12 = [('a',1)]

e13 :: Int -> Int -> Int
e13 x y = x + y * y