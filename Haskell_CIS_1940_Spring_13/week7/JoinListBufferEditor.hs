module Main2 where

import JoinList
import Editor    
import Scrabble
import Sized
import Buffer (Buffer(fromString))

initialBuffer :: JoinList (Score, Size) String
initialBuffer = fromString "This is initial buffer for notes"

main :: IO ()
main = runEditor editor initialBuffer