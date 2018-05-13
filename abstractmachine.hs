-- From G.Hutton book

data Expr = Val Int
          | Add Expr Expr

type Cont = [Op]
data Op = EVAL Expr
        | ADD Int

-- execute a control stack in the context of an integer argument
exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD n : c) m = exec c (n+m)

-- evaluate an expression in the context of a control stack
eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVAL y : c)

value :: Expr -> Int
{-
value (Val n) = n
value (Add x y) = value x + value y
-}
value e = eval e []

main = do
  putStrLn $ show $ value $ Add (Add (Val 2) (Val 3)) (Val 4)
