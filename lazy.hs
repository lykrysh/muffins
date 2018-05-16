-- From G.Hutton book
-- examples of lazy (call-by-name) evaluations

-- these funcs will run forever

ones :: [Int]
ones = 1 : ones

primes :: [Int]
primes = sieve [2..]

sieve :: [Int] -> [Int]
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

square :: Num a => a -> a
square n = n*n

sumWith_r:: Int -> [Int] -> Int
sumWith_r v [] = v
sumWith_r v (x:xs) = sumWith_r (v+x) xs

sumWith_l:: Int -> [Int] -> Int
sumWith_l v [] = v
sumWith_l v (x:xs) = (sumWith_l $! (v+x)) xs


main = do
  putStrLn $ show $ head ones
  putStrLn $ show $ take 3 ones
  putStrLn $ show $ replicate 3 1
  putStrLn $ show $ replicate 3 1
  -- putStrLn $ show $ filter (<= 5) [1..] Don't do this!!!
  putStrLn $ show $ takeWhile (<= 5) [1..] -- use takeWhile instead of filter
  putStrLn $ show $ take 10 primes
  putStrLn $ show $ takeWhile (< 10) primes

  -- default (call-by-name, lazy) evaluation
  putStrLn $ show $ square 10
  -- strict (call-by-value) evaluation
  putStrLn $ show $ square $! 10

  putStrLn $ show $ sumWith_r 0 [1,2,3]
  putStrLn $ show $ sumWith_l 0 [1,2,3]
