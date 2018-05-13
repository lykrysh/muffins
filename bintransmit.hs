-- Binary string transmitter example
-- from G.Hutton book

import Data.Char
type Bit = Int

{-
bin2int :: [Bit] -> Int
bin2int bits = sum [w*b | (w,b) <- zip weights bits]
  where weights = iterate (*2) 1
-}

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n`div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

-- each char to Unicode, then to 8-bit binary
encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

main = do
  putStrLn $ show $ bin2int [1,0,1,1]
  putStrLn $ show $ int2bin 13
  putStrLn $ show $ make8 [1,0,1,1]
  putStrLn $ show $ encode "abc"
  putStrLn $ decode [1,0,0,0,0,1,1,0]
  putStrLn $ transmit "higher-order functions are easy"
