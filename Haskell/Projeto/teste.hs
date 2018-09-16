splitAll :: (Eq a) => a -> [a] -> [[a]]
splitAll e l = foldl (\acc@(n:ns) x -> if x /= e then else ) [[]] l
