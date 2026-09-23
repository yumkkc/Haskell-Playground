import Data.Char (generalCategory)
import Data.Bifunctor (Bifunctor(first))
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [1..]

-- Exercise 2
fib' :: Integer -> Integer -> [Integer]
fib' a b = a : fib' b (a+b)

fibs2 :: [Integer]
fibs2 = fib' 0 1

-- Streams
data Stream t = S t (Stream t)

streamToList :: Stream a -> [a]
streamToList (S v vs) = v : streamToList vs

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList


-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat a = S a $  streamRepeat a

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (S a rst) = S (f a) $ streamMap f rst

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f v = S v $ streamFromSeed f (f v)

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0


interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams s1 s2 = go s1 s2 True
  where go (S x1 x1s) s2 True = S x1 $ go x1s s2 False
        go s1 (S x2 x2s) False = S x2 $ go s1 x2s True

nPowerStream :: Integer -> Stream Integer
nPowerStream n = streamFromSeed (*n) 1

powerOfTwoStream :: Stream Integer
powerOfTwoStream = nPowerStream 2

zerosStream :: Stream Integer
zerosStream = streamRepeat 0

ruler :: Stream Integer
ruler = interleaveStreams zerosStream (streamMap (+1) ruler)
