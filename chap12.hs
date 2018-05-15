-- From G.Hutton book

import Control.Applicative
import Control.Monad
import Data.Char

-- (1) idea of Functor

{-
inc :: [Int] -> [Int]
inc [] = []
inc (n:ns) = n+1 : inc ns
-}

inc :: Functor f => f Int -> f Int
inc = fmap (+1)
--inc = mymap (+1)

mymap :: (a -> b) -> [a] -> [b]
mymap f [] = []
mymap f (x:xs) = f x : map f xs
-- this is what Functor/fmap does = mapping over

-- let's implement my Functor instance
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap g (Leaf x) = Leaf (g x)
  fmap g (Node l r) = Node (fmap g l) (fmap g r)

{-
main = do
  putStrLn $ show $ inc [1,2,3]
  putStrLn $ show $ inc $ Just 1
  putStrLn $ show $ fmap (+1) Nothing
  putStrLn $ show $ fmap (*2) $ Just 3
  putStrLn $ show $ fmap length $ Leaf "abc"
  putStrLn $ show $ fmap even $ Node (Leaf 1) (Leaf 2)
  putStrLn $ show $ inc $ Node (Leaf 1) (Leaf 2)
-}

-- (2) idea of Applicative Functor & pure

-- return all possible ways of multiplying elements of 2 lists
prods :: [Int] -> [Int] -> [Int]
prods xs ys = pure (*) <*> xs <*> ys
--same as list comprehension: prods xs ys = [x*y | x <- xs, y <- ys]

{-
main = do
  putStrLn $ show $ pure (+1) <*> Just 1
  putStrLn $ show $ pure (+) <*> Just 1 <*> Just 2
  putStrLn $ show $ pure (+) <*> Nothing <*> Just 2
  putStrLn $ show $ pure (+1) <*> [1,2,3]
  putStrLn $ show $ pure (+) <*> [1,2,3] <*> [4,5]
  putStrLn $ show $ prods [1,2,3] [4,5]
-}

-- (3) idea of Monad & bind >>= operator

data Expr = Val Int | Div Expr Expr

-- check division by zero
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv n m = Just (m `div` m)

eval  :: Expr -> Maybe Int
eval (Val n) = Just n 
{-
eval (Div x y) = case eval x of
                   Nothing -> Nothing
                   Just n -> case eval y of
                               Nothing -> Nothing
                               Just m -> safeDiv n m
-- rewrite above using bind
eval (Div x y) =
  eval x >>= \n ->
    eval y >>= \m ->
      safeDiv n m
-}
-- also same as this
eval (Div x y) = do
  n <- eval x
  m <- eval y
  safeDiv n m

-- the list monad

pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = do
  x <- xs
  y <- ys
  return (x,y)
-- this is also the same as list comprehension
-- pairs xs ys = [(x,y) | x <- xs, y <- ys]

-- the state monad

type State = Int
--type ST = State -> State
newtype ST a = S (State -> (a,State))

app :: ST a -> State -> (a,State)
app (S st) x = st x

instance Functor ST where
  -- fmap :: (a -> b) -> ST a -> ST b
  fmap g st = S (\s -> let (x,s') = app st s in (g x, s'))

instance Applicative ST where
  -- pure :: a -> ST a
  pure x = S (\s -> (x,s))
  -- (<*>) :: ST (a -> b) -> ST a -> ST b
  stf <*> stx = S (\s ->
    let (f,s') = app stf s
        (x,s'') = app stx s' in (f x, s''))

instance Monad ST where
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f = S (\s -> let (x,s') = app st s in app (f x) s')

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _) n = (Leaf n, n+1)
rlabel (Node l r) n = (Node l' r', n'')
  where
    (l',n') = rlabel l n
    (r',n'') = rlabel r n'

fresh :: ST Int
fresh = S (\n -> (n,n+1))

alabel :: Tree a -> ST (Tree Int)
{-
alabel (Leaf _) = do
  n <- fresh
  return (Leaf n)
alabel (Node l r) = do
  l' <- mlabel l
  r' <- mlabel r
  return (Node l' r')
-}
alabel (Leaf _) = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r

-- more examples of generic monadic functions
-- already in Control.Monad
{-
mapM :: Monad m => (a -> m b) -> [a] m [b]
mapM f [] = return []
mapM f (x:xs) = do
  y <- f x
  ys <- mapM f xs
  return (y:ys)

filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM p [] = return []
filterM p (x:xs) = do
  b <- p x
  ys <- filterM p xs
  return (if b then x:ys else ys)

join :: Monad m => m (m a) -> m a
join mmx = do
  mx <- mmx
  x <- mx
  return x
-}

-- to use mapM function
conv :: Char -> Maybe Int
conv c | isDigit c = Just (digitToInt c)
       | otherwise = Nothing

main = do
  putStrLn $ show $ eval (Div (Val 4) (Val 3))
  putStrLn $ show $ eval (Div (Val 4) (Val 0))
  putStrLn $ show $ pairs [1,2] [3,4]
  putStrLn $ show $ pairs [1,2,3] [4,5,6,7,8]
  putStrLn $ show $ fst (rlabel tree 0)
  putStrLn $ show $ fst (app (alabel tree) 0)
  putStrLn $ show $ mapM conv "1234"
  putStrLn $ show $ mapM conv "123a4"
  putStrLn $ show $ filterM (\x -> [True,False]) [1,2,3]
  putStrLn $ show $ join [[1,2],[3,4],[5,6]]
  putStrLn $ show $ join (Just (Just 1))
