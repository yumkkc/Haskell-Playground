-- Validate credit card
-- 1
toDigitsRev :: Integer -> [Integer]
toDigitsRev n 
  | n <= 0    = []
  | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)


toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

-- 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (doubleEveryOther' (reverse xs) False)
    where
        doubleEveryOther' :: [Integer] -> Bool -> [Integer]
        doubleEveryOther' [] _ = []
        doubleEveryOther' (x:xs) True = 2 * x : doubleEveryOther' xs False
        doubleEveryOther' (x:xs) False = x : doubleEveryOther' xs True

-- 3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = (sum . toDigitsRev) x + sumDigits xs

-- 4
validate :: Integer -> Bool
validate n = (sumDigits . doubleEveryOther . toDigits) n `mod` 10 == 0

-- hanoi problem

type Peg = String
type Move  = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 source dest sparse = [(source, dest)]
hanoi n source dest sparse = hanoi (n-1) source sparse dest ++ [(source, dest)] ++ hanoi (n-1) sparse dest source


-- testing
test :: (Show b, Eq b) => String -> (a -> b) -> a -> b -> IO()
test name f input expected 
    | output == expected = print (name ++ ": Passed")
    | otherwise = print (name ++ ": Failed. Output [ " ++ show output ++ "]. Expected [" ++ show expected ++ "]")
    where 
        output = f input


main :: IO()
main = do
    test "toDigitsRev" toDigitsRev 1890 [0,9,8,1]
    test "doubleEveryOther" doubleEveryOther [8,7,6,5]  [16,7,12,5]
    test "sumDigits" sumDigits [16,7,12,5] 22
    test "validate" validate 4012888888881881 True
    test "validate" validate 4012888888881882  False
