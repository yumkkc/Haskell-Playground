{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where
import Employee
import Data.Tree
import Data.List

-- 1
glCons :: Employee -> GuestList -> GuestList
glCons e (GL el f) = GL (e:el) (empFun e + f)

-- 2
instance Semigroup GuestList where
  (<>) :: GuestList -> GuestList -> GuestList
  (<>) (GL el1 fs1) (GL el2 fs2) = GL (el1 ++ el2) (fs1+fs2)

instance Monoid GuestList where
  mempty :: GuestList
  mempty = GL [] 0

-- 3
moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ fs1) gl2@(GL _ fs2)
  | fs1 >= fs2 =  gl1
  | otherwise  =  gl2

-- ====testing====
em1 = Emp "abc" 1
em2 = Emp "bcd" 3
em3 = Emp "random" 10

gl1 = glCons em1 mempty
gl2 = glCons em2 mempty
gl3 = glCons em3 mempty

gl4 = gl1 <> gl2
mostFunGl = moreFun gl3 gl4

-- Exercise 2
-- data Tree a = Node a [Tree a]
treeFold :: Monoid b => (a -> b -> b) -> Tree a -> b
treeFold f (Node a tr) = f a $ foldr (\nt -> (<> treeFold f nt)) mempty tr


treeFold' :: Monoid b => (a -> [b] -> b) -> Tree a -> b
treeFold' f (Node a tr) = f a $ foldr (\nt ex -> treeFold' f nt : ex) mempty tr

-- test of treeFold
instance Semigroup Fun where
  (<>) = (+)
instance Monoid Fun where
  mempty = 0

sumFun = treeFold (\e -> (+ empFun e)) testCompany2
glTest = treeFold glCons testCompany2

-- the above, treeFold (glCons) testCompany2 works because in this case
-- b in (a -> b -> b) is a GuestList which is already a monoid
-- so in foldr which doing '<>' it uses its monoid and its mempty which is just "GL 0 []"
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel emp gls = (glCons emp empOnly, mixEmps)
  where empOnly = foldr (\(_,y) acc -> y <> acc) mempty gls
        mixEmps = foldr (\(x,y) acc -> moreFun x y <> acc) mempty gls


maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold' nextLevel

printGuestList :: GuestList -> String
printGuestList (GL emlist score) = unlines ( scoreStr : sort names)
  where names = map empName emlist
        scoreStr = "Total fun: " ++ show score


-- Exercise 5
main :: IO ()
main = readFile "company.txt"
       >>= return . read
       >>= return . maxFun
       >>= return . printGuestList
       >>= putStrLn
