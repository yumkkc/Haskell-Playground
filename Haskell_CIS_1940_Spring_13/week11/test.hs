module Test where

import Control.Applicative

-- (*) <$> [4,5] <*> pure 2
-- we essentially become? [4*, 5*]
-- now with <*> we do,
-- [8, 10]


-- now, (+) <$> ((*) <$> [4,5] <*> pure 2) <*> [6,1]
-- now, (+) <$> [8,10] <*> [6,1]
-- now, [\x -> 8+x, \x -> 10+x] <*> [6,1]
-- now, [14, 9, 16, 11]



newtype ZipList2 a = ZipList2 { getZipList :: [a] }
  deriving (Eq, Show, Functor)

instance Applicative ZipList2 where
  pure = ZipList2 . repeat
  ZipList2 fs <*> ZipList2 xs = ZipList2 (zipWith ($) fs xs)



-- ((-> )e) (a -> b) <*> ((-> e) a)
-- f :: (e -> (a -> b)) <*> x :: (e -> a)
-- \e -> (f e) (x e)
-- \e -> (a -> b) a
-- \e -> b


-- (a -> b) -> (e -> a) -> (e -> b)
-- this is for a functor of type ((->) e)

(*>.) :: Applicative f => f a -> f b -> f b
(*>.) fa fb = fb

-- for maybe it takes two and return second

-- for Pair, also same thing. gives second pair

-- this is basically lifting a function to a list
mapA' :: Applicative f => (a -> f b) -> [a] -> f [b]
mapA' _ [] = pure []
mapA' fa (x:xs) = (:) <$> fa x <*> mapA' fa xs

liftedColon = liftA (:)
mapA'' :: Applicative f => (a -> f b) -> [a] -> f [b]
mapA'' fa (x:xs) =liftedColon <$> fa x <*> mapA'' fa xs
