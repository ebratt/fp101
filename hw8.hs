--homework8
import Control.Monad

putStr'          :: String -> IO ()
putStr' []       = return ()
putStr' (x : xs) = 
    do putChar x 
       putStr' xs

putStrLn'    :: String -> IO ()
putStrLn' [] = putChar '\n'
putStrLn' xs = putStr' xs >> putStrLn' ""
    
getLine' :: IO String
getLine' = get []

get :: String -> IO String
get xs
  = do x <- getChar
       case x of
           '\n' -> return xs
           _ -> get (xs ++ [x])


interact' :: (String -> String) -> IO ()
interact' f
  = do input <- getLine'
       putStrLn' (f input)

sequence_' :: Monad m => [m a] -> m ()
sequence_' ms = foldr (>>) (return()) ms

sequence' :: Monad m => [m a] -> m [a]
sequence' [] = return []
sequence' (m:ms) 
  = do a <- m
       as <- sequence' ms
       return (a:as)

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f [] = return []
mapM' f (a:as)
  = f a >>= 
      \ b -> 
        do bs <- mapM' f as
           return (b:bs)

keepSmall :: Int -> IO Bool  
keepSmall x  
    | x < 4     = do putStrLn' ("Keeping " ++ show x)
                     return True  
    | otherwise = do putStrLn' (show x ++ " is too large, throwing it away")
                     return False  

showSmall :: Int -> IO Int
showSmall x  
    | x < 4     = do putStrLn' ("Keeping " ++ show x)
                     return x
    | otherwise = do putStrLn' (show x ++ " is too large, throwing it away")
                     return x

filterM'          :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
filterM' _ []     =  return []
filterM' p (x:xs) =  do
   flg <- p x
   ys  <- filterM' p xs
   return (if flg then x:ys else ys)

--foldLeftM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
--foldLeftM f seed xs = lgo seed xs
--              where
--                lgo seed [] = seed
--                lgo seed (x:xs) = lgo (f seed x) xs


foldl'          :: (b -> a -> b) -> b -> [a] -> b
foldl' f z0 xs0 = lgo z0 xs0
             where
                lgo z []     =  z
                lgo z (x:xs) = lgo (f z x) xs

liftM' :: Monad m => (a -> b) -> m a -> m b
liftM' f m = m >> \ a -> return (f a)