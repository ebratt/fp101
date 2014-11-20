test :: Int -> Int -> Int
test m 0 = 1
test m n = m * test m (n - 1)

concat2 :: [[a]] -> [a]
concat2 [] = []
concat2 (xs : xss) = xs ++ concat2 xss

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x <= y then x : merge xs (y : ys) else y : merge (x : xs) ys

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort zs) (msort ys)
  where (ys, zs) = halve xs