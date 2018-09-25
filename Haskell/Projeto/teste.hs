splitall :: (Eq a) => a -> [a] -> [[a]]
splitall sep [] = [[]]
splitall sep (x:xs)
   | x==sep = []:(a:as)
   | otherwise = (x:a):as
   where  (a:as) = splitall sep xs 
