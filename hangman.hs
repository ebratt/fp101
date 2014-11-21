--Hangman is a kids game whereby two players are involved: one player picks
--a word and the other player tries to guess the word by filling-in blanks.
--The guesser only gets so many guesses before the game is over. If the
--guesser guesses the work before the maximum number of attempts, he/she wins
--the game. If not, he/she loses.

import System.IO

-- getCh is a function that has some IO side-effect and returns a Char
-- reads a character without echo-ing it to the screen
getCh :: IO Char
getCh = 
    do hSetEcho stdin False -- disables echo-ing
       c <- getChar         -- not a mutable variable, just a binding
       hSetEcho stdin True  -- enables echo-ing
       return c

-- diff is a function that takes a String and returns a function that takes 
-- another String and returns a String with side-effects
-- elem is a library function that checks to see if an element
-- exists in a list and if it does then it returns True
-- so we are comparing the xs to the ys and we return each
-- x in xs where x is an element in ys
diff :: String -> String -> String
diff xs ys = 
    [if elem x ys then x else '-' | x <- xs]


-- guess is a function that takes a string and returns nothing
-- it has side-effects only
-- recursively loops for guesses until the player has the right answer
guess :: String -> Int -> Int -> IO ()
guess word attempts guesses = 
    if attempts == 0 then
      putStrLn "> You are out of guesses!"
    else
      do putStr "> "
         xs <- getLine
         if xs == word then
           do let success = "You got it in " ++ show guesses ++ " guesses!"
              putStrLn success
         else
           do let guessesleft = "" ++ (show (attempts-1)) ++ " guesses left..."
              let message = (diff word xs) ++ "\n" ++ guessesleft
              putStrLn message
              guess word (attempts-1) (guesses+1)

-- reads a line of text, echoing each character as a "_"
sgetLine :: IO String
sgetLine = 
    do x <- getCh
       if x == '\n' then
         do putChar x
            return []
       else
         do putChar '-'
            xs <- sgetLine
            return (x:xs)

-- hangman is a function that returns the empty tuple, or "void"
-- it has no return value and only has side-effects
hangman :: Int -> IO ()
hangman attempts = 
    do putStrLn "Think of a word: "
       word <- sgetLine
       let test = "Try to guess it in " ++ show attempts ++ " tries: "
       putStrLn test
       guess word attempts 1