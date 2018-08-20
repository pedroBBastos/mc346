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
    where split' e [] acc = [[]]
          split' e (x:xs) acc
              | x == e = acc:xs
              | otherwise = split' e xs (acc++[x])
