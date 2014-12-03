type Pos = (Int, Int)

origin :: Pos
origin = (0,0)

left :: Trans
left (x, y) = (x-1, y)

-- types can have parameters too!
type Pair a = (a, a)

mult :: Pair Int -> Int
mult (m, n) = m*n

copy :: a -> Pair a
copy x = (x, x)

-- type declarations can be nested!
type Trans = Pos -> Pos

-- however, they cannot be recursive
--type Tree = (Int, [Tree])

-- completely new types can be defined using (algebraic) data declarations
-- these are like Scala's Case Classes
-- False and True are called "constructors"
--data Bool = False | True

data Answer = Yes | No | Unknown
  deriving Show

answers :: [Answer]
answers = [Yes,No,Unknown]

flip         :: Answer -> Answer
flip Yes     = No
flip No      = Yes
flip Unknown = Unknown

-- shape is like an abstract base class in java
-- the constructors behave like functions themselves
data Shape = Circle Float 
           | Rect Float Float
  deriving Show

square   :: Float -> Shape
square n = Rect n n

-- pattern-matching here is like dynamic dispatching in java
area            :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y

-- recursive types (type synonyms are not allowed!)
-- when you want a recursive type, it has to be a nominal type
data Nat = Zero | Succ Nat
  deriving Show

nat2int          :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat          :: Int -> Nat
int2nat 0        = Zero
int2nat n        = Succ (int2nat (n - 1))

add Zero n = n
add (Succ m) n = Succ (add m n)


-- lets look at some more interesting types, like arithmetic expressions
data Expr = Val Int
          | Add Expr Expr
          | Mul Expr Expr

size :: Expr -> Int
size (Val n) = 1
size (Add x y) = size x + size y
size (Mul x y) = size x + size y

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

-- binary tree example 
data Tree = Leaf Int
          | Node Tree Int Tree
  deriving Show

occurs :: Int -> Tree -> Bool
occurs m (Leaf n) = m==n
occurs m (Node l n r) 
  | m==n = True
  | m<n = occurs m l
  | m>n = occurs m r

flatten :: Tree -> [Int]
flatten (Leaf n) = [n]
flatten (Node l n r) = flatten l
                    ++ [n]
                    ++ flatten r
-- this example is a search tree because, when we flatten it, we get a sorted list
t1 = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))