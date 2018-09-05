-- tamanho de uma lista
tamanho :: [a] -> Int
tamanho l = foldl (\acc _ -> acc + 1) 0 l

-- soma dos elementos de uma lista
somaElementos :: (Num a) => [a] -> a
somaElementos l = foldl (\acc x -> acc + x) 0 l

-- soma dos números pares de uma lista
somaElementosPares :: (Integral a) => [a] -> a
somaElementosPares l = somaElementos (filter (even) l)

-- soma dos elementos nas posições pares da lista ( o primeiro elemento esta na posicao 1)
soma :: (Num b) => [b] -> b
soma l = foldl (+) 0 (pegaPosicoesPares l)

pegaPosicoesPares :: (Num b) => [b] -> [b]
pegaPosicoesPares [] = []
pegaPosicoesPares [a] = []
pegaPosicoesPares (x:y:xs) = y : (pegaPosicoesPares xs)

-- soma2 :: (Num b) => [b] -> b
-- soma2 l = foldl (\acc x -> if (fst x) `mod` 2 then acc + (snd x)) 0 (formataLista l)
--     where formataLista l =

-- existe item na lista (True ou False)
existeElemento :: (Eq a) => [a] -> a -> Bool
existeElemento l e = foldl (\acc x -> if x == e then True else acc) False l

-- posição do item na lista (0 se nao esta la, 1 se é o primeiro)
-- posicaoElementoNaLista :: (Eq a) => [a] -> a -> Int

-- conta quantas vezes o item aparece na lista (0 se nenhuma)
qtdAparicoes :: (Eq a) => [a] -> a -> Int
qtdAparicoes l e = tamanho (filter (e==) l)

-- maior elemento de uma lista
maiorElemento :: (Ord a) => [a] -> a
maiorElemento [] = error "Nao  ha elementos nesta lista"
maiorElemento l = foldl (\acc e -> if e > acc then e else acc) (head l) l

-- reverte uma lista
reverterLista :: [a] -> [a]
reverterLista l = foldl (\acc x -> x:acc) [] l

-- intercala1 [1,2,3] [4,5,6,7,8]
--  ==> [1,4,2,5,3,6]

-- intercala1 :: [a] -> [a] -> [a]



-- intercala2 [1,2,3] [4,5,6,7,8]
--  ==>  [1,4,2,5,3,6,7,8]
