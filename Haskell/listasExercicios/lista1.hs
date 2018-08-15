-- tamanho de uma lista
tamanho :: (Integral b) => [a] -> b
tamanho [] = 0
tamanho (x:xs) = 1 + (tamanho xs)

-- soma dos elementos de uma lista
soma :: (Num a) => [a] -> a
soma [] = 0
soma (x:xs) = x + soma xs

-- soma dos números pares de uma lista
somaPares :: (Integral a, Eq a) => [a] -> a
somaPares [] = 0
somaPares (x:xs)
    | (x `mod` 2) == 0 = x + somaPares xs
    | otherwise = somaPares xs

-- soma dos elementos nas posições pares da lista
somaPosicaoPar :: (Integral a, Eq a) => [a] -> a
somaPosicaoPar [] = 0
somaPosicaoPar [e] = e
somaPosicaoPar (x:xs) = x + (somaPosicaoPar (tail xs))

-- existe item na lista (True ou False)
existe :: (Eq a) => [a] -> a -> Bool
existe [] _ = False
existe (x:xs) e
    | x == e = True
    | otherwise = existe xs e

-- posição do item na lista (0 se nao esta la, 1 se é o primeiro)
posicaoNaLista :: (Eq a, Integral b) => [a] -> a -> b
posicaoNaLista [] _ = 0
posicaoNaLista (x:xs) e
    | x == e = 1
    | otherwise = 1 + (posicaoNaLista xs e)

-- conta quantas vezes o item aparece na lista (0 se nenhuma)
qtdAparicoes :: (Eq a, Integral b) => [a] -> a -> b
qtdAparicoes [] _ = 0
qtdAparicoes (x:xs) e
    | x == e = 1 + (qtdAparicoes xs e)
    | otherwise = qtdAparicoes xs e

-- maior elemento de uma lista - FAZER p/ proxima aula - variáveis locais
maiorElemento :: (Ord a) => [a] -> a
maiorElemento [] = error "Nao ha nada nesta lista meu sr."
maiorElemento [a] = a
maiorElemento (x:xs)
    | x > (maiorElemento xs) = x
    | otherwise = maiorElemento xs

-- reverte uma lista - FAZER p/ próxima aula - recursão com acumulados
reverterLista :: [a] -> [a]
reverterLista [] = []
reverterLista (x:xs) = (reverterLista xs) ++ x:[]

-- intercala 2 listas (intercala1 e intercala2)
intercala1 :: [a] -> [a] -> [a]
intercala1 [] _ = []
intercala1 _ [] = []
intercala1 (x:xs) (y:ys) = x:y:(intercala1 xs ys)

intercala2 :: [a] -> [a] -> [a]
intercala2 [] ys = ys
intercala2 xs [] = xs
intercala2 (x:xs) (y:ys) = x:y:(intercala2 xs ys)

-- a lista ja esta ordenada?
estaOrdenada :: (Ord a) => [a] -> Bool
estaOrdenada [] = True
estaOrdenada [a] = True
estaOrdenada (x:xs)
    | x > (head xs) = False
    | otherwise = estaOrdenada xs

-- dado n gera a lista de 1 a n
geraAteN :: (Integral a) => a -> [a]
geraAteN 1 = 1:[]
geraAteN n = geraAteN (n-1) ++ n:[]

-- shift right
-- shiftr [1,2,3,4]
--  ==> [4,1,2,3]
-- shiftRight :: [a] -> [a]
-- shiftRight [] = []
-- shiftRight [a] = [a]
-- shiftRight (x:xs) = x : shiftRight (tail xs)

-- shift left
-- shiftr [1,2,3,4]
--  ==> [2,3,4,1]
shiftLeft :: [a] -> [a]
shiftLeft [] = []
shiftLeft (x:xs) = xs ++ x:[]



-- [1,2,3,4,5,5,666,7], 5, 99
-- [1,2,3,4,99,99,666,7]
