module Parsing where

import Data.Char
import Control.Monad

infixr 5 +++

-- The monad of parsers

-- a Parser of type a is a function that takes an input string and 
-- produces a list of results, each of which is a pair comprising
-- a result value of type a and an output string
newtype Parser a = P (String -> [(a, String)])

instance Monad Parser where
    return v     = P (\inp -> [(v, inp)])
    p >>= f      = P (\inp -> case parse p inp of
                                 []         -> []
                                 [(v, out)] -> parse (f v) out)

instance MonadPlus Parser where
    mzero        = P (\inp -> [])
    p `mplus` q  = P (\inp -> case parse p inp of
                                 []         -> parse q inp
                                 [(v, out)] -> [(v, out)])

-- Basic parsers
-- failure is a function that always fails (opposite of return)
failure          :: Parser a
failure          = mzero

-- item is a function that fails if the input string is empty and
-- succeeds with the first character as the result value otherwise
item             :: Parser Char
item             = P (\inp -> case inp of 
                                 []     -> []
                                 (x:xs) -> [(x, xs)])

-- parse is a function that takes a Parser of type a and returns a 
-- function that takes a String as input and returns a list of pairs
-- of a and String
parse            :: Parser a -> String -> [(a, String)]
parse (P p) inp  = p inp

-- Choice
-- a function that applies the first parser to the input string, and
-- if that fails it applies the second instead, read as "or else"
(+++)            :: Parser a -> Parser a -> Parser a
p +++ q          = p `mplus` q

-- Derived primitives
-- a parser for single characters that satisfy the predicate p
sat              :: (Char -> Bool) -> Parser Char
sat p            = do x <- item
                      if p x then return x else failure

digit            :: Parser Char
digit            = sat isDigit

lower            :: Parser Char
lower            = sat isLower

upper            :: Parser Char
upper            = sat isUpper

letter           :: Parser Char
letter           = sat isAlpha

alphanum         :: Parser Char
alphanum         = sat isAlphaNum

char             :: Char -> Parser Char
char x           = sat (== x)

string           :: String -> Parser String
string []        = return []
string (x:xs)    = do char x
                      string xs
                      return (x:xs)

many             :: Parser a -> Parser [a]
many p           = many1 p +++ return []

many1            :: Parser a -> Parser [a]
many1 p          = do v <- p
                      vs <- many p
                      return (v:vs)

ident            :: Parser String
ident            = do x <- lower
                      xs <- many alphanum
                      return (x:xs)

nat              :: Parser Int
nat              = do xs <- many1 digit
                      return (read xs)

int              :: Parser Int
int              = do char '-'
                      n <- nat
                      return (-n)
                    +++ nat

space            :: Parser ()
space            = do many (sat isSpace)
                      return ()


-- Ignoring spacing
token            :: Parser a -> Parser a
token p          = do space
                      v <- p
                      space
                      return v

identifier       :: Parser String
identifier       = token ident

natural          :: Parser Int
natural          = token nat

integer          :: Parser Int
integer          = token int

symbol           :: String -> Parser String
symbol xs        = token (string xs)

