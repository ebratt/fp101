module Countdown where

{-
   Given a sequence of numbers and a target number, attempt to 
   construct an expression whose value is the target, by combining 
   one or more numbers from the sequence using addition, subtraction,
   multiplication, division, and parentheses.

   Ex: given sequence = 1, 3, 7, 10, 25, 50
       target         = 765
       one solution   = (1 + 50) * (25 - 10)

       ***there are 780 different solutions***
-}

-- Define a type for the four numeric operators
data Op = Add | Sub | Mul | Div | Exp
  deriving Show
{-instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"-}

-- Define a function that decides if the application of an operator
-- to two positive naturals gives another positive natural.
valid         :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 1 && y /= 0 && x `mod` y == 0
valid Exp x y = y /= 1 && x > 0 && y >= 0

-- Define a function that actually performs the valid application.
apply         :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y

-- Define a type for numeric expressions, which can either be an 
-- integer value or the application of an operator to two argument
-- expressions.
data Expr = Val Int | App Op Expr Expr
  deriving Show

-- Define a list of values in an expression.
values             :: Expr -> [Int]
values (Val n)     = [n]
values (App _ l r) = values l ++ values r

-- Define a function that returns the overall value of an expression,
-- provided that this value is a positive natural number.
eval             :: Expr -> [Int]
eval (Val n)     = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, 
                                  y <- eval r, 
                                  valid o x y]

-- Define a function that returns all subsequences of a list, which 
-- are given by all possible combinations of excluding or including
-- each element.
subs        :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs

-- Define a function that returns all possilbe ways of inserting a 
-- new element into a list.
interleave          :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys):map (y:) (interleave x ys)

-- Define a function that returns all permutations of a list, which 
-- are given by all possible reorderings of the elements.
perms        :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

-- Define a function that returns all choices from a list, which are 
-- given by all possible ways of selecting zero or more elements in 
-- any order.
choices    :: [a] -> [[a]]
choices xs = concat (map perms (subs xs))

-- Define a function that formalizes what it means to solve an 
-- instance of the countdown problem.
solution        :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

-- Define a function that returns all possible ways of splitting a 
-- list into two non-empty lists that append to give the original
-- list.
split        :: [a] -> [([a], [a])]
split []     = []
split [_]    = []
split (x:xs) = ([x], xs) : [(x:ls, rs) | (ls, rs) <- split xs]

-- Define a function that combines each pair of expressions using
-- each of the four numeric operators.
ops :: [Op]
ops = [Add, Sub, Mul, Div, Exp]

combine     :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

-- Define a key function which returns all possible expressions
-- whose list of values is precisely a given list.
exprs     :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns  = [e | (ls, rs) <- split ns,
                 l <- exprs ls,
                 r <- exprs rs,
                 e <- combine l r]

-- Define a function that returns all possible expressions that
-- solve an instance of the countdown problem, by first generating
-- all expressions over each choice from the given list of numbers,
-- and then selecting those expressions that successfully evaluate
-- to give the target.
solutions      :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns,
                      e <- exprs ns',
                      eval e == [n]]

-- Declare a type of expressions that evaluate successfully paired
-- with their overall values.
type Result = (Expr, Int)

-- Define a function that combines each pair of expressions using
-- each of the four numeric operators.
combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops,
                                                   valid o x y]

-- Define a function that returns all possible results comprising
-- expressions whose list of values is precisely a given list.
results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [res | (ls, rs) <- split ns,
                    lx <- results ls,
                    ry <- results rs,
                    res <- combine' lx ry]

-- Define a function that returns all possible expressions that solve
-- an instance of the countdown problem, by first generating all 
-- results over each choice from the given numbers, and then 
-- selecting those expressions whose value is the target.
solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns,
                       (e, m) <- results ns',
                       if (ns' == []) then m == n + 1 || m == n - 1
                       else m == n]

-- ex, head (solutions' [1,3,7,10,25,50] 765) returns...
--   App Mul (Val 3) (App Sub (App Mul (Val 7) (App Sub (Val 50) (Val 10))) (Val 25))
--   ...applying Sub (Val 50) (Val 10)...
--   App Mul (Val 3) (App Sub (App Mul (Val 7) (Val 40)) (Val 25))
--   ...applying Mul (Val 7) (Val 40)...
--   App Mul (Val 3) (App Sub (Val 280) (Val 25))
--   ...applying Sub (Val 280) (Val 25)...
--   App Mul (Val 3) (Val 255)
--   ...applying Mul (Val 3) (Val 255)...
--   Val 765
--
-- ex, last (solutions' [1,3,7,10,25,50] 765) returns...
--   App Div (App Mul (Val 50) (App Add (Val 25) (App Exp (App Sub (Val 3) (Val 1)) (Val 7)))) (Val 10)
--   ...applying Sub (Val 3) (Val 1)...
--   App Div (App Mul (Val 50) (App Add (Val 25) (App Exp (Val 2) (Val 7)))) (Val 10)
--   ...applying Exp (Val 2) (Val 7)...
--   App Div (App Mul (Val 50) (App Add (Val 25) (Val 128))) (Val 10)
--   ...applying Add (Val 25) (Val 128)...
--   App Div (App Mul (Val 50) (Val 153)) (Val 10)
--   ...applying Mul (Val 50) (Val (153)...
--   App Div (Val 7650) (Val 10)
--   ...applying Div (Val 7650) (Val 10)...
--   Val 765

