-- http://learnyouahaskell.com/a-fistful-of-monads
-- Using `filter` (without using lists as a monad)

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = filter onBoard
  [(c+2, r-1), (c+2, r+1), (c-2, r-1), (c-2, r+1), (c+1, r-2), (c+1, r+2), (c-1, r-2), (c-1, r+2)]
    where onBoard (c,r) = c `elem` [1..8] && r `elem` [1..8]

in3 :: KnightPos -> [KnightPos]
in3 start = do
  first <- moveKnight start
  second <- moveKnight first
  moveKnight second

-- same as
-- in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start

main = do
  putStrLn $ show $ moveKnight (6,2)
  putStrLn $ show $ moveKnight (8,1)
  putStrLn $ show $ canReachIn3 (6,2) (6,1)
  putStrLn $ show $ canReachIn3 (8,1) (7,3)
