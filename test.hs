-- setup some test data
l1 = ['t','h','i','s',' ','i','s',' ','a',' ','l','i','s','t',' ','o','f',' ','c','h','a','r','s']
l2 = [['a','b'],['c','d','e']]
t1 = ("Yes", True, 'a', 1)

-- Doubles an integer: 
double x = x + x
-- Quadruples an integer: 
quadruple x = double (double x)
-- Factorial of a positive integer:
fact n = 
    if n >= 0 
        then product [1..n]
    else
        0


-- Average of a list of integers:
average ns = div (sum ns) (length ns)

--qsort takes a list of xs with a head x and sorts the elements in ascending order:
{-
qsort[] = []
qsort(x:xs) = qsort smaller ++ [x] ++ qsort larger
              where
              	smaller = [a | a <- xs, a <= x]
              	larger = [b | b <- xs, b > x]
-}

-- Make a list of 0 to n:
zeroto :: Int -> [Int]
zeroto n = [0..n]

-- Mult takes three integers, one at a time, and returns their product:
mult :: Int -> (Int -> (Int -> Int))
mult x y z = x * y * z

