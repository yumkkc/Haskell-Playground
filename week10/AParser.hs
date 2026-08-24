module AParser where

import           Control.Applicative

import           Data.Char
import Data.Char (digitToInt)

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

-- Exercise 1
first :: (a -> b) -> (a, c) -> (b, c)
first f (x, rst) = (f x, rst)

-- newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- if i just do p, then i expect a String -> Maybe (a, String)
-- if i do . p, the the second function of composition wants a tyupe of "Maybe (a, String)"
-- we know maybe is already a functor, so we can apply that
-- and we have a function called first :: (a -> b) -> (a, c) -> (b, c)

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p =  Parser $ (first f `fmap`) . runParser p


-- testing
charParser :: Parser Char
charParser = satisfy isDigit

-- using this lets convert the parser that checks if its a digit, to the one which actually spits out a digit

newParser :: Parser Int
newParser = digitToInt `fmap` charParser


instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser $ \x -> Just (a , x)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) p1 p2 = Parser $ \x -> case runParser p1 x of
      Nothing -> Nothing
      Just (f, rst) -> first f <$> runParser p2 rst


data Employee = Employee String String
  deriving (Show)

nameParser = Parser (\x -> Just (take 3 x, x))
phoneParser = Parser (\x -> Just (take 3 $ drop 3 x, x))


employeeParser = Employee <$> nameParser <*> phoneParser