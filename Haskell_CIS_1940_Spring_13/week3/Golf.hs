module Golf where

    import Data.Maybe
    import Data.List
-- 1import Distribution.Simple.Program (findProgramOnSearchPath)
    
    everyNth :: [a] -> Int -> [a]
    everyNth xs n = map fst $ filter (\(_,i) -> i `mod` n == 0) (zip xs [1..])

    skips :: [a] -> [[a]]
    skips xs = map (everyNth xs) [1..(length xs)]

-- 2 (Local maxima)
    safeTail :: [a] -> Maybe [a]
    safeTail [] = Nothing
    safeTail (_:xs) = Just xs

    localMaxima :: [Integer] -> [Integer]
    localMaxima xs = map (\(_,b,_) -> b) $ filter (\(a,b,c) -> a < b && b > c) (zip3 xs xs1 xs2)
      where xs1 = fromMaybe [] (safeTail xs)
            xs2 = fromMaybe [] (safeTail xs1)

-- 3
    -- histogram :: [Integer] -> String
    {-
    Group by occurances with type [[Integer]], where positiion determines indicies n-1ith index
    [1,2,1,3] => [2,1,1,0,0,0]
    for this we sort first
    -}
    increment :: [(Integer, Integer)] -> Integer -> [(Integer, Integer)]
    increment [] n = [(n,1)]
    increment p@((x,v):xs) n = if x == n then (x,v+1):xs else (n,1):p

    findMagnitude :: [Integer] -> [(Integer, Integer)]
    findMagnitude xs = foldl increment [] $ sort xs
    
    construct ::  [(Integer, Integer)] -> Integer -> String
    construct xs n = concat $ map (\x -> if fromMaybe 0 (lookup x xs) >= n then "*" else " ") [0..9] ++ ["\n"]

    constructIt :: [(Integer, Integer)] -> Integer -> [String]
    constructIt xs n = reverse $ map (construct xs) [1..n]

    maxMagnitude :: [(Integer, Integer)] -> Integer
    maxMagnitude xs = maximum $ map snd xs

    histogram :: [Integer] -> String
    histogram xs = concat $ constructIt magList (maxMagnitude magList) ++ ["==========\n"] ++ ["0123456789\n"]
                 where magList = findMagnitude xs
    



-- for test
    test_everyNth_1 = everyNth "Saiyam" 2 == ['a', 'y', 'm']
    test_everyNth_2 = everyNth [True, False, True, False] 2 == [False, False]
    test_skip1 = skips "ABCD" == ["ABCD", "BD", "C", "D"]
    test_skip2= skips "ABCD" == ["ABCD", "BD", "C", "D"]
    test_skip3 = skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
    test_skip4= skips [1] == [[1]]
    test_skip5 = skips [True,False] == [[True,False], [False]]
    test_skip6 = null (skips [])

    test_maxima1 = localMaxima [2,9,5,6,1] == [9,6]
    test_maxima2 = localMaxima [2,3,4,1,5] == [4]
    test_maxima3 = localMaxima [1,2,3,4,5] == []

    main :: IO()
    main = do
        print test_everyNth_1
        print test_everyNth_2
        print test_skip1
        print test_skip2
        print test_skip3
        print test_skip4
        print test_skip5
        print test_skip6
        print test_maxima1
        print test_maxima2
        print test_maxima3
