-- From G.Hutton book

import System.IO

data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Int
          | App Op Expr Expr

instance Show Expr where
  show (Val n) = show n
  show (App o l r) = brak l ++ show o ++ brak r
    where
      brak (Val n) = show n
      brak e = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

-- return all subsequences of a list
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
  where yss = subs xs

-- return all possible ways of inserting new elem
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

-- return all permutations/reordering
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat $ map (interleave x) (perms xs)

choices :: [a] -> [[a]]
choices = concat . map perms . subs

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n =
  elem (values e) (choices ns) && eval e == [n]

split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

ops :: [Op]
ops = [Add,Sub,Mul,Div]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- split ns,
              l <- exprs ls,
              r <- exprs rs,
              e <- combine l r]

solutions :: [Int] -> Int -> [Expr]
solutions ns n =
  [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

{-
main = do
  putStrLn $ show $ App Add (Val 1) (App Mul (Val 2) (Val 3))
  putStrLn $ show $ eval $ App Add (Val 2) (Val 3)
  putStrLn $ show $ eval $ App Sub (Val 2) (Val 3)
  putStrLn $ show $ subs [1,2,3]
  putStrLn $ show $ interleave 1 [2,3,4]
  putStrLn $ show $ perms [2,3,4]
  putStrLn $ show $ choices [2,3,4]
  putStrLn $ show $ split [2,3,4]
-}

-- output all possible expressions over each given numbers that yield the target
-- brute force approach
main :: IO ()
main = print $ solutions [1,3,7,10,25,50] 765
