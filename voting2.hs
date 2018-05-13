-- From G.Huton book

import Data.List

votes :: [String]
votes = ["Red","Blue","Green","Blue","Blue","Red"]

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

result :: Ord a => [a] -> [(Int, a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

ballots :: [[String]]
ballots = [
  ["Red","Green"], 
  ["Blue"],
  ["Green","Red","Blue"],
  ["Blue","Green","Red"],
  ["Green"]]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

-- no!
winner :: Ord a => [a] -> a
winner = snd . last. result

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
               [c] -> c
               (c:cs) -> winner' (elim c bs)

main = do
  putStrLn $ show $ rank ballots
  putStrLn $ show $ winner ballots
  putStrLn $ show $ winner' ballots
