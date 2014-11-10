-- higher order functions are functions that take functions as arguments or return functions as results
twice :: (a -> a) -> a -> a
twice f x = f (f x)

