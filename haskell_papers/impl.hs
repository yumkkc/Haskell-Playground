{-# LANGUAGE LambdaCase #-}

import Data.Char ( isDigit )

newtype Parser a = Parser (String -> [(a, String)])

runParser :: Parser a -> (String -> [(a, String)])
runParser (Parser f) = f

applyFirst :: (a -> b) ->  (a, c) -> (b, c)
applyFirst f (fst,snd) = (f fst, snd)

item :: Parser Char
item = Parser (\case
                ""    -> []
                (c:cs) -> [(c,cs)])

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = Parser $ \cs -> map (applyFirst f) $ runParser p cs


            
-- testing            
checkDigitParser :: Parser Bool
checkDigitParser = isDigit <$> item

instance Applicative Parser where
    pure :: a -> Parser a
    pure a = Parser $ \cs -> [(a, cs)]

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (<*>) fp p = Parser $ \cs -> concatMap (\(f,rst) -> runParser (f <$> p) rst ) $ runParser fp cs

checkDigitParser2 :: Parser Bool
checkDigitParser2 = pure isDigit <*> item

-- we can see that checkDigitParser and checkDigitParse2 is same
-- this is the law that applicative and functor needs to fulfill
-- which is correct

instance Monad Parser where
    return = pure

    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p1 >>= p2 = Parser $ \cs -> concatMap (\(v,rst) -> runParser (p2 v) rst) $ runParser p1 cs