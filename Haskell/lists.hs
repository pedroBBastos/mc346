myLast l = last l

lastButOne l = last (init l)

elementAt l p = l !! p-1

myLength l = length l

isPalindrome l = if l == reverse l then True else False

-- k-th element
nEsimoElemento :: [a] -> Int -> a
nEsimoElemento [] _ = error "A lista eh vazia meu sr."
nEsimoElemento _ 0 = error "Aqui a gente comeca com 1 (primeiro elemento)"
nEsimoElemento fullList@(x:xs) n
      | n >= length fullList = error "O nesimo elemento pedido nao existe pq excede o tamanho da lista."
      | n == 1 = x
      | otherwise = nEsimoElemento xs (n-1)

-- flatten a nested list structure
-- de acordo com o exercicio, teremos que definir um novo tipo para NestedLists, pois em Haskell listas sao homogeneas
-- flatten :: [a] -> [a]
-- flatten [] = []
-- flatten [x] = x:[]
-- flatten (x:xs) = x : flatten xs
-- flatten ((x:xs):xss) = x : flatten xs : flatten xss

-- Eliminate consecutive duplicates of list elements
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs)
      | x `elem` xs = x : compress (removeReplicate xs x)
      | otherwise = x : compress xs
      where
        removeReplicate [] _ = []
        removeReplicate fullRest@(y:ys) p
            | y /= p = y : removeReplicate ys p
            | otherwise = removeReplicate ys p

-- testando eliminacao de repetido
testando :: (Eq a) => [a] -> a -> [a]
testando [] _ = []
testando fullRest@(y:ys) p
      | y /= p = y : testando ys p
      | otherwise = testando ys p

-- Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs)
      | x `elem` xs = (x : findAllAndPack xs x) : pack (removeContiguous xs x)
      | otherwise = [x] : pack xs
      where
        findAllAndPack [] _ = []
        findAllAndPack fullRest@(y:ys) p
            | y == p = y : findAllAndPack ys p
            | otherwise = []
        removeContiguous [] _ = []
        removeContiguous fullRest@(y:ys) p
            | y == p = removeContiguous ys p
            | otherwise = fullRest

-- Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding
-- data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode fullList@(x:xs)
      | x `elem` xs = (countElement fullList x, x) : encode (removeContiguous xs x)
      | otherwise = (1, x) : encode xs
      where
        countElement [] _ = 0
        countElement otherFullList@(x:xs) p
            | x == p = 1 + (countElement xs p)
            | otherwise = 0
        removeContiguous [] _ = []
        removeContiguous fullRest@(y:ys) p
            | y == p = removeContiguous ys p
            | otherwise = fullRest
