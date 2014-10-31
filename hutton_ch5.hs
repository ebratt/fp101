(^!) :: Int -> Int -> Int
m ^! 0 = 1
m ^! n = m * (m ^! (n - 1))

nd      :: [Bool] -> Bool
nd []     = True
nd (x:[]) = x
nd (x:xs) = x && (nd xs)

replicate2 :: Int -> a -> [a]
replicate2 0 _ = []
replicate2 n x = x : (replicate2 (n-1) x)

select          :: [a] -> Int -> a
select [] _     = error "list is empty"
select (x:xs) 0 = x
select (x:xs) n = select xs (n-1)

elem2 :: Eq a => a -> [a] -> Bool
elem2 _ [] = False
elem2 x (y:ys) = if x == y then True else elem2 x ys
