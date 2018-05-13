qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]

seqn :: Monad m => [m a] -> m [a]
--seqn :: [IO a] -> IO [a]
seqn [] = return []
seqn (act:acts) = do
  x <- act
  xs <- seqn acts
  return (x:xs)

main = do
  putStrLn $ show $ qsort [5,4,6,3,7,2]
  seqn [getChar, getChar, getChar]
