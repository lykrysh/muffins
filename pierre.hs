-- http://learnyouahaskell.com/a-fistful-of-monads

type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
  | abs ((left + n) - right) < 4 = Just (left + n, right)
  | otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
  | abs (left - (right + n)) < 4 = Just (left, right + n)
  | otherwise = Nothing

routine1 :: Maybe Pole
routine1 = case landLeft 1 (0,0) of
            Nothing -> Nothing
            Just pole1 -> case landRight 4 pole1 of
                            Nothing -> Nothing
                            Just pole2 -> case landLeft 2 pole2 of
                                            Nothing -> Nothing
                                            Just pole3 -> landLeft 1 pole3

routine2 :: Maybe Pole
routine2 = do
  pole0 <- return (0,0)
  pole1 <- landLeft 1 pole0
  pole2 <- landRight 4 pole1
  pole3 <- landLeft 2 pole2
  landLeft 1 pole3

main = do
  putStrLn $ show $ return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2
  putStrLn $ show $ return (0,0) >>= landLeft 1 >>= landRight 4 >>= landLeft (-1) >>= landRight (-2)
  putStrLn $ show $ return (0,0) >>= landLeft 1 >> Nothing >>= landRight 1
  putStrLn $ show routine1
  putStrLn $ show routine2
