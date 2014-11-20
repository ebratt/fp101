module Monads where

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
(Monads.Just x) >>= f = f x

pairs       :: [a] -> [b] -> [(a,b)]
pairs xs ys = do x <- xs
                 y <- ys
                 return (x, y)


data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show 

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

type State = Int

data ST a = S (State -> (a, State))

apply            :: (a -> Maybe b) -> Maybe a -> Maybe b
apply f Nothing  = Nothing
apply f (Just x) = f x

instance Monad ST where
      -- return :: a -> ST a
      return x  =  \s -> (x,s)

      -- (>>=)  :: ST a -> (a -> ST b) -> ST b
      st >>= f  =  \s -> let (x,s') = st s in f x s'

fresh :: ST Int
fresh = S (\n -> (n, n+1))


beep :: IO ()
beep = putStr "\BEL"

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs


seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (a:as) = do a
                 seqn as