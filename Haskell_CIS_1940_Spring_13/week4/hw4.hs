import Data.List
-- 1
-- Conversion to wholemeal programming

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x : xs)
  | even x  = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' xs = foldr (\x y -> (x - 2) * y) 1 (filter even xs)

-- 2
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' n = sum $ takeWhile (>0) (filter even (iterate f n))
  where f 1 = 0
        f n' | even n' = n' `div` 2
             | otherwise = 3 * n' + 1

-- testing for 100 numbers
test1 = foldr (\x y -> x && y) True (map (\x -> fun2 x == fun2' x) (takeWhile (<1000) [1..]))


-- Exercise 2 : Folding with trees

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

createNewNode :: a -> Tree a
createNewNode v = Node 0 Leaf v Leaf

insertToTree :: a -> Tree a -> Tree a
insertToTree v Leaf = createNewNode v
insertToTree v (Node h Leaf v' Leaf) = Node (h+1) (createNewNode v) v' Leaf
insertToTree v (Node h lst v' rst) = Node h' lst' v' rst'
  where
    getRootHeight :: Tree a -> Integer
    getRootHeight Leaf = -1
    getRootHeight (Node h _ _ _) = h

    go | getRootHeight lst > getRootHeight rst = (lst, insertToTree v rst)
           | otherwise   = (insertToTree v lst , rst)
    (lst', rst') = go
    h' = max (getRootHeight lst') (getRootHeight rst') + 1

foldTree :: [a] -> Tree a
foldTree = foldr insertToTree Leaf

-- Exercise 3
-- implement xor with flodr
-- explaination : i make true when i find "True" but when i encounter another "True", its False since now its even
xor :: [Bool] -> Bool
xor = foldr f False
  where f False b = b
        f True False = True
        f True True = False

-- implement map with foldr
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) []

-- implement foldr with foldl
myfoldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
myfoldl f base xs = foldr (\x y t -> y (f t x)) id xs base

-- Exercise 4 : Finding primes
-- given n, generate odd primes number up to 2n + 2.

removeUnwanted :: Integer -> [Integer]
removeUnwanted n = filter (`notElem` unwanted) [1..n]
  where unwanted = [k | k <- [1..n], i <- [1..n], j <- [i..n], (i + j + 2 * i * j) <= n, (i + j + 2 * i * j) == k]

sieveSundaram' :: Integer -> [Integer]
sieveSundaram' n = map (\x -> 2 * x + 1) (removeUnwanted n)

-- another appraoch
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+ 1 ) . (* 2)) $ [1..n] \\ matches
  where matches = map (\(i,j) -> i + j + 2 * i * j) $
                  filter (\(i,j) -> i + j + 2 * i * j <= n) $
                  cartProd [1..n] [1..n]


cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]
