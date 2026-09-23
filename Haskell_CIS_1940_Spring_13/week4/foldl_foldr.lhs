foldr : (a -> b -> b) -> b -> [a] -> b
foldl :: (b -> a -> b) -> b -> [a] -> b

take a list [a, b , c], z as accumulator and function f1 and f2 where
f1 :: (a -> b -> b)
f2 :: (b -> a -> b)


foldr => (a `f` (b `f` (c `f` z)))

foldl => (((z `f` a ) `f` b) `f` c)

implement foldl with foldr


approach 1 :
==========================
changing the f


(((z `f` a ) `f` b) `f` c)

=> f (f (f (z a) b) c)

thinking of building closure after closure till we apply to base z using foldr

acc' :: (b -> b)

f :: (b -> a -> b)

a -> b -> (b -> b)

\a b -> (\b -> f b a)

====
(\b -> f b a)

\a b -> (\b -> f (\b -> f b a) a)

            a : 3       a : 10
\b -> f (\b -> f b a) a

f :: (a + b)

given b = 0,

f (f 0 3) 10
f 3 10
13

(a -> b -> b)

wrong-foldl' :: (b -> a -> b) -> b -> [a] -> b
wrong-foldl' f z a = (foldr (\x y -> (\t -> f (y t) x)) (\x -> x) a) z

f :: \x -> x
[1,2,3]

\t -> (+ ((+ ((+ (f t) 3) t) 2 ) t) 1)

====================================
Proper Explanation

we use the concept of closure. Looking at foldl, accumulator is applied at the first with x1 and then to the rest.
Also, the accumualtor is the first argument in foldl unlike foldr. So, we build closure around it.

we create a accumulator of type: (b -> b) where b is the type of original accumulator. This says
given an accumualtor, we give u accumulator. We initialize this with identity lambda,

(\x -> x)

Our foldr function will be:

(\x y -> (\t -> f (y t) x))

foldr expects (a -> b -> b), taking accumulator as second argument i.e.,

x is our element to fold, y is our accumulator (in this case the closure to be applied later)

we build a new lambda :: \t -> f (y t) x

where we expect t (of type b) and then apply that to our closure y
and then the result of that to our original function with current x.

At the end, we get a function like

\t -> f (\t -> f (y t) x) x ...

and so on, with a type b -> b, meaning it expects an accumualtor and then applies all of these lambda in the order

its wrong

> wrongFoldl' :: (b -> a -> b) -> b -> [a] -> b
> wrongFoldl' f z a = (foldr (\x y -> (\t -> f (y t) x)) (\x -> x) a) z

Why is this wrong?

this behaves like foldr. We build a closure and immediately apply.

\t -> f (y t) x

we are applying closure from the right itself.

We have to flip this up

\t -> y (f t x)

this hence will be like =>

\t -> (\t -> y (f t x)) (f t x)

if we have [1,2] with function as (+)
then
                    [2]     [1]
\t -> (\t -> y (f t x)) (f t x)

when we pass t as 0

(\t -> (\x -> x) (+ t 2)) (+ 0 1)
(\x -> x) (+ 1 2)
3

The above shows that + 0 1 occurs first and then its result is then applied to 2.
this happens because function argument are evaluated first!!!


> foldl' :: (b -> a -> b) -> b -> [a] -> b
> foldl' f z a = (foldr (\x y -> (\t -> y (f t x))) (\x -> x) a) z

better fold

> myfoldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
> myfoldl f z a = foldr (\x y t -> y (f t x)) id a z
