module Test where

> type Name = String

> data Employee = Employee { name  ::  Name
>                          , phone :: String}
>                deriving Show

if we have name or Name and phone of String, we can build Employee
but lets say we have Maybe Name and Mayabe String (for phone)
if we have Nothing we cannot build Employee but if Just Name and Just String,
we can make the Employee

essentially this means is:
turn our function into this
(Name -> String -> Employee) -> (Maybe Name -> Maybe String -> Maybe Employee)

(Name -> String -> Employee) -> ([Name] -> [String] -> [Employee])

what if we want to write the above??
esentially we are taking some data constructor and wrapping it around.

we have (e -> Name) and (e -> String)
can we convert this to (e -> Employee) since it can be build from the function above
(Name -> String -> Employee) -> ( (e -> Name) -> (e -> String) -> (e -> Employee))


Generalizing
===========================
(a -> b -> c) -> (f a -> f b -> fc)

this is similar to typeclass

fmap :: (a -> b) -> (f a -> f b)

lets say its fmap2

fmap2 :: Functor f => (a -> b -> c) -> (f a -> f b -> f c)
fmap2 h fa fb = undefined


Accoring to fmpa we get

fmap (a -> (b -> c)) f a

fmap f (b -> c) f b

we can apply this to Applicative. Apply in {"context"} of a functor
h <$> fa <*> fb

this just says that

h is a function of (a -> b -> c) which is just (a -> (b -> c))

it applyies all a in the Functor context of f to this h which gives us

f (b -> c) which then is applied by <*> (apply) giving us the value of f c

> class Functor f => Applicative2 (f :: * -> *) where
>   pure  :: a -> f a
>   (<*$*>) :: f (a -> b) -> f a -> f b


> instance Applicative2 Maybe where
>   pure              = Just
>   Nothing <*$*> _     = Nothing
>   _ <*$*> Nothing     = Nothing
>   Just f <*$*> Just x =  Just (f x)

> nm = Just "saiyam"
