{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
import Text.Read

-- 1
parseMessage' :: [String] -> LogMessage
parseMessage' [] = Unknown ""
parseMessage' p@("I" : n : ns) = case (readMaybe n :: Maybe Int) of
                                Nothing -> Unknown (unwords p)
                                Just timeStamp -> LogMessage Info timeStamp (unwords ns)
parseMessage' p@("W" : n : ns) = case (readMaybe n :: Maybe Int) of
                                Nothing -> Unknown (unwords p)
                                Just timeStamp -> LogMessage Warning timeStamp (unwords ns)
parseMessage' p@("E" : n1 : n2 : ns) = case (readMaybe n1 :: Maybe Int) of
                                        Nothing -> Unknown (unwords p)
                                        Just line -> case (readMaybe n2 :: Maybe Int) of
                                            Nothing -> Unknown (unwords p)
                                            Just timeStamp -> LogMessage (Error line) timeStamp (unwords ns)
parseMessage' p = Unknown (unwords p)                                            
                             

parseMessage :: String -> LogMessage                             
parseMessage line = parseMessage' (words line)

parse :: String -> [LogMessage]
parse logs = map parseMessage (lines logs)

-- 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert p Leaf = Node Leaf p Leaf
insert p@(LogMessage _ timestamp _) (Node left q@(LogMessage _ value _) right) = 
    if timestamp <= value then Node (insert p left) q right
    else Node left q (insert p right)
insert _ tree = tree

-- 3
build :: [LogMessage] -> MessageTree
build logs = foldr insert Leaf logs


-- 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left x right) = inOrder left ++ (x : inOrder right)

-- 5
searchMoreThanThat :: Int -> [LogMessage] -> [LogMessage]
searchMoreThanThat _ [] = []
searchMoreThanThat key p@((LogMessage _ t _) : xs) = if t > key then p else searchMoreThanThat key xs
searchMoreThanThat _ rst = rst

unparseMessage :: LogMessage -> String
unparseMessage (LogMessage (Error a) b rst) = unwords ["E", show a, show b, rst]
unparseMessage (LogMessage Info b rst) = unwords ["I", show b, rst]
unparseMessage (LogMessage Warning b rst) = unwords ["W", show b, rst]
unparseMessage (Unknown message) = message

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logs = map unparseMessage (searchMoreThanThat 49 sorted_logs)
                     where sorted_logs = inOrder (build logs)

-- just for tests
main :: IO()
main = do
    --print (parseMessage "E 2 562 help help")
    --print (unparseMessage (parseMessage "E 2 562 help help"))
    -- print (parseMessage "E 562 help help")
    -- print (parseMessage "I 29 la la")
    -- print (parseMessage "This is not in the right format")
--    print (insert (parseMessage "I 2 Completed armadillo processing") (build (parse "I 6 Completed armadillo processing\nI 1 Nothing to report\nI 4 Everything normal")))
--    print (inOrder (insert (parseMessage "I 2 Completed armadillo processing") (build (parse "I 6 Completed armadillo processing\nI 1 Nothing to report\nI 4 Everything normal"))))
    print (whatWentWrong (parse "I 50 Completed armadillo processing\nI 1 Nothing to report\nI 4 Everything normal"))