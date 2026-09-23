Folds

> data Tree a = Empty
>             | Node (Tree a) a (Tree a)
>  deriving (Show, Eq)

> leaf :: a -> Tree a
> leaf x = Node Empty x Empty


> treeSize :: Tree a -> Integer
> treeSize Empty = 0
> treeSize (Node l _ r) = 1 + treeSize l + treeSize r

> treeSum :: Tree Integer -> Integer
> treeSum Empty            = 0
> treeSum (Node l x r)     = x + treeSum l + treeSum r

> treeDepth :: Tree a -> Integer
> treeDepth Empty = 0
> treeDepth (Node l _ r) = 1 + max (treeDepth l) (treeDepth r)


In all of the above cases, we apply some function over some value and the return of left and right subtree


> treeFold :: b -> (b -> a -> b -> b) -> Tree a  -> b
> treeFold e _ Empty = e
> treeFold e f (Node l x r) = f (treeFold e f l) x (treeFold e f r)

with the above fold, lets define treeSize

> treeSize' :: Tree a -> Integer
> treeSize' = treeFold 0 (\x _ z -> 1 + x + z)

-- testing trees

> testTree = Node (Node Empty 12 Empty) 13 Empty


Lets generalize List

> data CustomList a = E
>                  | C a (CustomList a)
>  deriving (Show)

Building on to the above definition, the fold will be

> lenCustList :: CustomList a  -> Integer
> lenCustList E = 1
> lenCustList (C a xs) = (+) 1 $ lenCustList xs

> sumCustList :: Num a => CustomList a -> a
> sumCustList E = 0
> sumCustList (C a xs) = (+) a $ sumCustList xs


Observe above, we can extract things out

> foldCustList :: b -> (a -> b -> b) -> CustomList a -> b
> foldCustList b _ E = b
> foldCustList b f (C x xs) = f x $ foldCustList b f xs

Going by two above examples, its CLEAR that to define a folding abstraction over any datatype (most datatype)
we pass one argument per constructor for the datatype, saying how to handle it when it occurs.

MONOID
===================

> newtype NewList a = NewList [a]
>   deriving (Eq, Ord, Show, Semigroup)

> instance Monoid (NewList a) where
>   mempty = NewList []
>   mappend (NewList a) (NewList b) = NewList (a ++ b)
