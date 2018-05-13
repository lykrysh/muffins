-- http://learnyouahaskell.com/a-fistful-of-monads
-- Emulate filtering (in list comprehension) using the list monad

class Monad m => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a

instance MonadPlus [] where
  mzero = []
  mplus = (++)

guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero

sevensOnly :: [Int]
sevensOnly = do
  x <- [1..50]
  guard ('7' `elem` show x)
  return x

-- same as [x| x <- [1..50], '7' `elem` show x]

main =
  putStrLn $ show sevensOnly
