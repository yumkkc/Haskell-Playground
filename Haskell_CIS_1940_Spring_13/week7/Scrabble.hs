module Scrabble where

import Data.Char (toUpper)

newtype Score = Score Int
    deriving (Show, Ord, Eq, Num)

getScore :: Score -> Int
getScore (Score i) = i

instance Semigroup Score where    
    (<>) = (+)

instance Monoid Score where
    mempty = 0

score :: Char -> Score    
score c = case toUpper c of
    c | c `elem` ['A', 'E', 'I', 'O', 'U', 'L', 'N' , 'R', 'S', 'T'] -> 1
    c | c `elem` ['D', 'G']                 -> 2
    c | c `elem` ['B', 'C', 'M', 'P']       -> 3
    c | c `elem` ['F', 'H', 'V', 'W', 'Y']  -> 4
    c | c `elem` ['J', 'X']                 -> 8
    c | c `elem` ['Q', 'Z']                 -> 10
    'K'                                     -> 5
    _                                       -> 0

scoreString :: String -> Score
scoreString = foldr (\x y -> score x <> y ) (Score 0)

