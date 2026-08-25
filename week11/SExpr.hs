{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char (isUpper, isSpace, isAlpha, isAlphaNum, isDigit)

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = ((:) <$> p <*> zeroOrMore p) <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> (zeroOrMore p <|> pure [])

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

-- basically have to convert Parser c -> Parser [c]
spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
ident = (++) <$> oneOrMore (satisfy isAlpha) <*> zeroOrMore (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving (Show)

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving (Show)


parseAtom :: Parser Atom
parseAtom = N <$> (spaces *> posInt) <|> I <$> (spaces *> ident)

openParen :: Parser Char
openParen = char '('

closedParen :: Parser Char
closedParen = char ')'

parseSExpr :: Parser SExpr
parseSExpr = Comb <$> (spaces *> openParen *> spaces *> zeroOrMore parseSExpr <* spaces <* closedParen) <|> A <$> parseAtom
