-- From G.Hutton book

import Data.Char

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']

count :: Char -> String -> Int
count c str = length $ filter (== c) str
-- count x xs = length [x' | x' <- xs, x == x']

lowers :: [Char] -> Int
lowers str = length $ filter (isLower) str
-- lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let $ (let2int c + n) `mod` 26
  | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
  where n = lowers xs

-- Chi-square statistic formula
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

-- table of approx percentage prequencies of letters
table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

crack :: String -> String
crack xs = encode (-factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0..25]]
    table' = freqs xs

main = do
  putStrLn $ show $ positions False [True, False, True, False]
  putStrLn $ show $ lowers "Haskell"
  putStrLn $ show $ count 's' "Mississippi"
  putStrLn $ show $ let2int 'b'
  putStrLn $ show $ int2let 0
  putStrLn $ show $ shift 3 'a'
  putStrLn $ show $ shift (-3) 'a'
  putStrLn $ show $ encode 3 "Haskell is fun."
  putStrLn $ show $ freqs "abbcccddddeeeee"
  putStrLn $ show $ rotate 3 [1,2,3,4,5]
  --putStrLn $ show $ [chisqr (rotate n table') table | n <- [0..25]] where table' = freqs "kdvnhoo lv ixq"
  putStrLn $ show $ crack "kdvnhoo lv ixq"
  putStrLn $ show $ crack "vscd mywzboroxcsyxc kbo ecopev"
  putStrLn $ show $ crack $ encode 3 "haskell"
  putStrLn $ show $ crack $ encode 3 "boxing wizards jump quickly"
