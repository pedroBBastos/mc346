-- posicoes - dado um item e uma lista, retorna uma lista com todas as posicoes (primeiro elemento esta na posicao 1) do item na lista
posicoes :: (Eq a, Integral b) => a -> [a] -> [b]
posicoes e lista = posicoes' e lista [] 0
    where posicoes' e [] acc _ = acc
          posicoes' e (x:xs) acc p
            | x == e = posicoes' e xs (acc ++ (p+1):[]) (p+1)
            | otherwise = posicoes' e xs acc (p+1)

-- split - dado um item e uma lista retorna uma lista de listas, todos os elementos da lista antes do item (a primeira vez que ele aparece) e todos depois
split :: (Eq a) => a -> [a] -> [[a]]
split elemento lista = split' elemento lista []
    where split' e [] acc = [acc]
          split' e (x:xs) acc
              | x == e = acc:[xs]  -- acc eh uma lista dos elementos que estao antes de 'e' -> Por isso eh colocada no inicio de uma nova lista de lista que tem como elemento o resto da lista original
              | otherwise = split' e xs (acc++x:[])

-- splitall - mesma coisa que o split mas retorna todas as sublistas
splitAll :: (Eq a) => a -> [a] -> [[a]]
splitAll elemento lista = splitAll' elemento lista []
    where splitAll' e [] acc = [acc]
          splitAll' e (x:xs) acc
              | x == e = acc:(splitAll' e xs [])
              | otherwise = splitAll' e xs (acc++x:[])

-- drop n lista - a lista sem os n primeiros elementos
dropNDaLista :: (Integral b) => b -> [a] -> [a]
dropNDaLista n lista = dropNDaLista' n lista 1
    where dropNDaLista' n [] _ = []
          dropNDaLista' n (x:xs) count
              | count == n = xs
              | otherwise = dropNDaLista' n xs (count+1)

-- take n lista - os primeiros n elementos da lista
takeNDaLista :: (Integral b) => b -> [a] -> [a]
takeNDaLista n lista = takeNDaLista' n lista []
    where takeNDaLista' n [] acc = acc
          takeNDaLista' 0 _ _ = []
          takeNDaLista' n (x:xs) acc
              | n == 1 = (acc ++ x:[])
              | otherwise = takeNDaLista' (n-1) xs (acc ++ x:[])
