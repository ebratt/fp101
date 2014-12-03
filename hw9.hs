module HW where

--week9 hw
import Data.List
import Data.Char
import Unsafe.Coerce

data Nat = Zero
         | Succ Nat
  deriving Show

natToInteger :: Nat -> Integer
natToInteger (Succ n) = 1 + natToInteger n
natToInteger Zero = 0

-- to enable n+k patterns, use 
--   :set -XNPlusKPatterns
integerToNat :: Int -> Nat
integerToNat 0 = Zero
integerToNat (n+1) = Succ (integerToNat n)

add :: Nat -> Nat -> Nat
add n Zero = n
add n (Succ m) = Succ (add m n)

mult            :: Nat -> Nat -> Nat
mult m Zero     = Zero
mult m n = add m (mult m (Succ n))

data Tree = Leaf Integer
          | Node Tree Tree
  deriving Show

balanced :: Tree -> Bool
leaves (Leaf _) = 1
leaves (Node l r) = leaves l + leaves r
balanced (Leaf _) = True
balanced (Node l r) 
    = abs (leaves l - leaves r) <= 1 && balanced l && balanced r

t1 = Node (Leaf 1) (Leaf 2)
t2 = Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4))
t3 = 
    Node (
        Node (
            Node (
                Node (
                    Leaf 1) (
                    Leaf 2)) (
            Node (
                Leaf 3) (
                Leaf 4))) (
        Node (
            Leaf 5) (
            Leaf 6))) (
    Node (
        Leaf 7) (
        Leaf 8))

balance          :: [Integer] -> Tree
halve xs         = splitAt (length xs `div` 2) xs
balance [x]      = Leaf x
balance xs       = Node (balance ys) (balance zs)
  where (ys, zs) = halve xs

data Expr = Add Expr Expr | Val Int
  deriving Show


class Monoid a where
    mempty :: a
    (<>) :: a -> a -> a

instance Monoid [a] where
    mempty = []
    (<>) = (++)

class Functor f where
    fmap :: (a -> b) -> f a -> f b

instance HW.Functor Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just a) = Just (f a)

class (HW.Functor f) => Foldable f where
    fold :: (HW.Monoid m) => f m -> m

instance HW.Foldable [] where
    fold = foldr (<>) mempty

--fold :: (Foldable f, Moinoid a) => f a -> a
