--week8.hs
-- type IO () is similar to the "void" return type in java


-- this function is purely imperative in that it does not return
-- any value - rather, it simply has a side effect of printing to
-- the console
strlen :: IO ()
strlen = do putStr "Enter a string: "
            xs <- getLine      -- xs has a different type than getLine
            putStr "The string has "
            putStr (show (length xs))
            putStrLn " characters"

