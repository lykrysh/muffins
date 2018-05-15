-- From G.Hutton book
-- how to use monoids

import Data.Foldable

newtype Sum a = Sum a deriving (Eq, Ord, Show, Read)

instance Num a => Monoid (Sum a) where
  -- mempty :: Sum a
  mempty = Sum 0
  -- mappend :: Sum a -> Sum a -> Sum a
  Sum x `mappend` Sum y = Sum (x+y)

getSum :: Sum a -> a
getSum (Sum x) = x

--

newtype Product a = Product a deriving (Eq, Ord, Show, Read)

instance Num a => Monoid (Product a) where
  mempty = Product 1
  Product x `mappend` Product y = Product (x*y)

getProduct :: Product a -> a
getProduct (Product x) = x

-- foldables

data Tree a = Leaf a | Node (Tree a)(Tree a) deriving Show

instance Foldable Tree where
  -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
  foldMap f (Leaf x) = f x
  foldMap f (Node l r) = foldMap f l `mappend` foldMap f r
  -- foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f v (Leaf x) = f x v
  foldr f v (Node l r) = foldr f (foldr f v r) l
  -- foldl :: (a -> b -> a) -> a -> Tree b -> a
  foldl f v (Leaf x) = f v x
  foldl f v (Node l r) = foldl f (foldl f v l) r

average :: Foldable t => t Int -> Int
average ns = sum ns `div` length ns

-- traversables
{-
-- it's built-in!
mytraverse :: (a -> Maybe b) -> [a] -> Maybe [b]
mytraverse g [] = pure []
mytraverse g (x:xs) = pure (:) <*> g x <*> mytraverse g xs
-}

instance Functor Tree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)

instance Traversable Tree where
  -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse g (Leaf x) = pure Leaf <*> g x
  traverse g (Node l r) = pure Node <*> traverse g l <*> traverse g r

dec :: Int -> Maybe Int
dec n = if n > 0 then Just (n-1) else Nothing


main = do
  putStrLn $ show $ mconcat [Sum 2, Sum 3, Sum 4]
  putStrLn $ show $ mconcat [Product 2, Product 3, Product 4]
  -- foldables
  putStrLn $ show $ getSum (foldMap Sum [1..10])
  putStrLn $ show $ getProduct (foldMap Product [1..10])
  putStrLn $ show $ length [1..10]
  putStrLn $ show $ length (Node (Leaf 'a') (Leaf 'b'))
  putStrLn $ show $ null (Leaf 1)
  putStrLn $ show $ foldl1 (+) (Node (Leaf 1) (Leaf 2))
  putStrLn $ show $ foldr1 (+) [1..10]
  putStrLn $ show $ average [1..10]
  -- Data.Foldable: and or all any concat
  putStrLn $ show $ and [True,False]
  putStrLn $ show $ or (Node (Leaf True) (Leaf False))
  putStrLn $ show $ any even (Node (Leaf 1) (Leaf 2))
  putStrLn $ show $ concat (Node (Leaf [1,2]) (Leaf [3]))
  -- traversables
  putStrLn $ show $ traverse dec [1,2,3]
  putStrLn $ show $ traverse dec (Node (Leaf 1) (Leaf 2))
  putStrLn $ show $ traverse dec (Node (Leaf 0) (Leaf 1))
  putStrLn $ show $ sequenceA [Just 1, Just 2, Just 3]
  putStrLn $ show $ sequenceA (Node (Leaf (Just 1)) (Leaf (Just 2)))
  putStrLn $ show $ sequenceA (Node (Leaf Nothing) (Leaf (Just 1)))
