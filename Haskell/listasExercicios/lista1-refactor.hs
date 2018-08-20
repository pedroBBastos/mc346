-- tamanho de uma lista
tamanho2 :: (Integral b) => [a] -> b
tamanho2 list = tamanho' list 0
    where tamanho' [] acc = acc -- caso base sempre retorna o acumulador
          tamanho' (x:xs) acc = tamanho' xs (acc+1) -- caso recursivo faz primiro a conta (acc+x) e so no final chama a recursao

-- soma dos elementos de uma lista
soma2 :: (Num a) => [a] -> a
soma2 [] = 0
soma2 (x:xs) = let ss = soma2 xs in x + ss

soma3 :: (Num a) => [a] -> a
soma3 lista = soma' lista 0
      where soma' [] acc = acc
            soma' (x:xs) acc = soma' xs (acc+x)

-- soma dos números pares de uma lista -->> esse acaba usando mais memoria. PQ??
somaPares2 :: (Integral a) => [a] -> a
somaPares2 lista = somaP lista 0
      where somaP [] acc = acc
            somaP (x:xs) acc
                | x `mod` 2 == 0 = somaP xs (acc+x)
                | otherwise = somaP xs acc
