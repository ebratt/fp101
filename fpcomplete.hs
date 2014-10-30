hailstone :: Integer -> Integer
hailstone n 
    | n `mod` 2 == 0 = n `div` 2
    | otherwise      = 3 * n + 1

fact :: Integer -> Integer
fact n
    | n < 0     = error "negative numbers not allowed!"
    | n == 0    = 1
    | otherwise = n * fact (n - 1)

foo :: Integer -> Integer
foo 0 = 16
foo 1
    | "Haskell" > "C++" = 3
    | otherwise         = 4
foo n
    | n < 0           = 0
    | n `mod` 17 == 2 = -43
    | otherwise       = n + 3

isEven :: Integer -> Bool
isEven n = n `mod` 2 == 0

-- pattern matching
sumPair :: (Int, Int) -> Int
sumPair (x,y) = x + y

-- more pattern matching
intListLength :: [Integer] -> Integer
intListLength [] = 0
intListLength (_:xs) = 1 + intListLength xs

sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo []         = []
sumEveryTwo (x:[])     = []
sumEveryTwo (x1:x2:xs) = x1 + x2 : sumEveryTwo xs

-- combining functions
hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

-- lazy evaluation of the hailstoneSeq means it doesn't get evaluated until its needed!
hailstoneLen :: Integer -> Integer
hailstoneLen n = intListLength (hailstoneSeq n) - 1

-- enumeration types
-- declares a new algebraic data type called Thing
data Thing = Shoe
    | Ship        -- data constructor for Ship
    | SealingWax  -- data constructor for SealingWax
    | Cabbage     -- data constructor for Cabbage
    | King        -- data constructor for King
  deriving Show   -- magical instruction to tell GHC to convert Things to Strings

shoe :: Thing
shoe = Shoe

listO'Things :: [Thing]
listO'Things = [Shoe, SealingWax, King, Cabbage, King]

-- write functions on Things by pattern matching (like case classes in Scala)
isSmall :: Thing -> Bool
isSmall Ship       = False
isSmall King       = False
isSmall _          = True

data FailableDouble = Failure
    | OK Double
  deriving Show

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d) = d

-- data constructors can have more than one argument
data Person = Person String Int Thing
            | Parent String Int Thing
            | Child String Int Thing
  deriving Show

brent :: Person
brent = Person "Brent" 30 SealingWax
stan :: Person
stan = Person "Stan" 94 Cabbage

mom :: Person
mom = Parent "Mom" 65 Ship

kid :: Person
kid = Child "Kid" 13 King

getAge :: Person -> Int
getAge (Person _ a _) = a

getThing :: Person -> Thing
getThing (Person _ a t) = t

getName :: Person -> String
getName (Person n _ _) = n

data AlgDataType = Constr1 Thing String
                 | Constr2 Thing
                 | Constr3 FailableDouble String Int
                 | Constr4
  deriving Show

adt :: AlgDataType
adt = Constr1 King "Test"

-- a pattern of the form x@pattern can be used to match a value against pattern, but also give
-- the name x to the entire value being matched:
baz :: Person -> String
baz p@(Person n _ _) = "The name field of (" ++ show p ++ ") is " ++ n

-- patterns can be nested:
checkFav :: Person -> String
checkFav (Person n _ SealingWax) = n ++ ", you're my kind of person!"
checkFav (Person n _ _)          = n ++ ", your favorite thing is lame."
checkFav (Parent n _ _)          = n ++ ", you are a parent."
checkFav (Child n _ _)           = n ++ ", you are a child."

-- case expressions are the fundamental construct for doing pattern-matching:
n = case "Hello" of
    []       -> 3
    ('H':s) -> length s
    _       -> 7

-- recursive data types:
data IntList = Empty | Cons Int IntList
  deriving Show

intListProd :: IntList -> Int
intListProd Empty = 1
intListProd (Cons x xs) = x * intListProd xs

data Tree = Leaf Char
          | Node Tree Int Tree
  deriving Show

tree :: Tree
tree = Node (Leaf 'x') 1 (Node (Leaf 'y') 2 (Leaf 'z'))

-- define functions for user-defined types
-- Map
{-addOneToAll :: IntList -> IntList
addOneToAll Empty = Empty
addOneToAll (Cons x xs) = Cons (x + 1) (addOneToAll xs)-}

myIntList = Cons 2 (Cons (-3) (Cons 5 Empty))

{-absAll :: IntList -> IntList
absAll Empty = Empty
absAll (Cons x xs) = Cons (abs x) (absAll xs)-}

{-squareAll :: IntList -> IntList
squareAll Empty = Empty
squareAll (Cons x xs) = Cons (x * x) (squareAll xs)-}

mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList _ Empty = Empty
mapIntList f (Cons x xs) = Cons (f x) (mapIntList f xs)

addOne x = x + 1
square x = x * x

addOneToAll xs = mapIntList addOne xs
absAll xs = mapIntList abs xs
squareAll xs = mapIntList square xs

-- Filter
keepOnlyEven :: IntList -> IntList
keepOnlyEven Empty = Empty
keepOnlyEven (Cons x xs)
    | even x    = Cons x (keepOnlyEven xs)
    | otherwise = keepOnlyEven xs

filterIntList :: (Int -> Bool) -> IntList -> IntList
filterIntList _ Empty = Empty
filterIntList p (Cons x xs)
    | p x       = Cons x (filterIntList p xs)
    | otherwise = filterIntList p xs

-- Fold

-- Polymorphism
data List t = E | C t (List t)
  deriving Show

lst1 :: List Int
lst1 = C 3 (C 5 (C 2 E))

lst2 :: List Char
lst2 = C 'x' (C 'y' (C 'z' E))

lst3 :: List Bool
lst3 = C True (C False E)

-- generalized form of filterIntList
filterList _ E = E
filterList p (C x xs)
    | p x       = C x (filterList p xs)
    | otherwise = filterList p xs

myList = C 2 (C (-3) (C 5 E))

-- generalized form of mapIntList
mapList :: (a -> b) -> List a -> List b
mapList f (C x xs) = C (f x) (mapList f xs)
mapList f E        = E

double x = 2 * x

emptyStringList :: [String]
emptyStringList = []

-- replacing partial functions
doStuff1 :: [Int] -> Int
doStuff1 []  = 0
doStuff1 [_] = 0
doStuff1 xs  = head xs + head (tail xs)

doStuff2 :: [Int] -> Int
doStuff2 []        = 0
doStuff2 [_]       = 0
doStuff2 (x1:x2:_) = x1 + x2

-- writing partial functions
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

-- using guarantees
data NonEmptyList a = NEL a [a]
  deriving Show

nelToList :: NonEmptyList a -> [a]
nelToList (NEL x xs) = x:xs

listToNEL :: [a] -> Maybe (NonEmptyList a)
listToNEL []     = Nothing
listToNEL (x:xs) = Just (NEL x xs)

headNEL :: NonEmptyList a -> a
headNEL (NEL x _) = x

tailNEL :: NonEmptyList a -> [a]
tailNEL (NEL _ xs) = xs

-- anonymous functions
{-gt100 :: Integer -> Bool
gt100 x = x > 100

greaterThan100 :: [Integer] -> [Integer]
greaterThan100 xs = filter gt100 xs-}

-- better way using an anon function
greaterThan100 :: [Integer] -> [Integer]
greaterThan100 xs = filter (\x -> x > 100) xs

foo2 :: (b -> c) -> (a -> b) -> (a -> c)
foo2 f g = \x -> f (g x)

myTest :: [Integer] -> Bool
myTest = even . length . greaterThan100

f :: Int -> Int -> Int
f x y = 2*x + y

foobar :: [Integer] -> Integer
foobar = sum . map (\x -> 7*x + 2) . filter (>3)

fold :: (a -> b -> b) -> b -> [a] -> b
fold f z [] = z
fold f z (x:xs) = f x (fold f z xs)

sum' = fold (+) 0
product' = fold (*) 1
length' = fold (\_ s -> 1 + s) 0

class Listable a where
    toList :: a -> [Int]

instance Listable Int where
    -- toList :: Int -> [Int]
    toList x = [x]

instance Listable Bool where
    toList True = [1]
    toList False = [0]

-- different implementation of foldl to make it NOT lazy
-- ex, running foldl (+) 0 [1..30000000] will run out of memory
-- but running foldl' (+) 0 [1..30000000] takes ~43 seconds
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ z [] = z
foldl' f z (x:xs) = let z' = f z x in z' `seq` foldl' f z' xs

-- another example of NON lazy...DO NOT RUN THIS - IT MAY CRASH GHCI!!!
(&&!) :: Bool -> Bool -> Bool
True &&! True   = True
True &&! False  = False
False &&! True  = False
False &&! False = False

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

result = if (3 < 5)
    then
        "yes"
    else
        "no"

result' = if' (3 < 5) "yes" "no"

-- dynamic programming example
--import Data.Array

knapsack :: [Double]   -- values 
           -> [Integer]  -- nonnegative weights
           -> Integer    -- knapsack size
           -> Double     -- max possible value
knapsack vs ws maxW = m!(numItems-1, maxW)
  where numItems = length vs
        m = array ((-1,0), (numItems-1, maxW)) $
              [((-1,w), 0) | w <- [0 .. maxW]] ++
              [((i,0), 0) | i <- [0 .. numItems-1]] ++
              [((i,w), best) 
                  | i <- [0 .. numItems-1]
                  , w <- [1 .. maxW]
                  , let best
                          | ws!!i > w  = m!(i-1, w)
                          | otherwise = max (m!(i-1, w)) 
                                            (m!(i-1, w - ws!!i) + vs!!i)
              ]

example = knapsack [3,4,5,8,10] [2,3,4,5,9] 20
