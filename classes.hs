data Foo = Foo { x :: Integer, str :: String }
instance Eq Foo where
    (Foo x1 str1) == (Foo x2 str2) = (x1 == x2) && (str1 == str2)

data Foo2 = Foo2 { x2 :: Integer, str2 :: String }
  deriving (Eq, Ord, Show)

foo :: (Num a, Show a, Show b) => a -> a -> b -> String
foo x y t = 
    show x ++ " plus " ++ show y ++ " is " ++ show (x+y) ++ ". " ++ show t

-- Location, in two dimensions
class Located a where
    getLocation :: a -> (Int, Int)

class (Located a) => Movable a where
    setLocation :: (Int, Int) -> a -> a

-- An example type, with accompanying instances.
data NamedPoint = NamedPoint {
    pointName :: String, 
    pointX    :: Int,
    pointY    :: Int
} deriving (Show)

instance Located NamedPoint where
    getLocation p = (pointX p, pointY p)

instance Movable NamedPoint where
    setLocation (x, y) p = p { pointX = x, pointY = y }

-- Moves a value of a Movable type by the specificied displacement.
-- This works for any movable, including NamedPoint
move :: (Movable a) => (Int, Int) -> a -> a
move (dx, dy) p = setLocation (x + dx, y + dy) p
  where
    (x, y) = getLocation p

myPoint = NamedPoint "point1" 0 0
myPoint2 = move (-10, 40) myPoint