{-module Monads where

-- monads, from graham hutton
inc        :: [Int] -> [Int]
inc []     = []
inc (n:ns) = n+1 : inc ns

sqr        :: [Int] -> [Int]
sqr []     = []
sqr (n:ns) = n^2 : sqr ns

data Expr = Val Int | Div Expr Expr
data Maybe a = Nothing | Just a
  deriving Show

apply :: (a -> Monads.Maybe b) -> Monads.Maybe a -> Monads.Maybe b
apply f Monads.Nothing = Monads.Nothing
apply f (Monads.Just x) = f x

eval           :: Expr -> Monads.Maybe Int
eval (Val n)   = Monads.Just n
eval (Div x y) = do n <- eval x
                    m <- eval y
                    safediv n m

safediv :: Int -> Int -> Monads.Maybe Int
safediv n m = if m == 0 then Monads.Nothing else Monads.Just (n `div` m)

seqn                                 :: Monads.Maybe a -> Monads.Maybe b -> Monads.Maybe (a,b)
seqn Monads.Nothing _                = Monads.Nothing
seqn _ Monads.Nothing                = Monads.Nothing
seqn (Monads.Just x) (Monads.Just y) = Monads.Just (x, y)

(>>=)                 :: Monads.Maybe a -> (a -> Monads.Maybe b) -> Monads.Maybe b
Monads.Nothing >>= _  = Monads.Nothing
(Monads.Just x) >>= f = f x-}

pairs       :: [a] -> [b] -> [(a,b)]
pairs xs ys = do x <- xs
                 y <- ys
                 return (x, y)

