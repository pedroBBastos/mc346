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

-- soma dos elementos nas posições pares da lista ( o primeiro elemento esta na posicao 1)
somaPosicaoPar :: (Integral a, Eq a) => [a] -> a
somaPosicaoPar [] = 0
somaPosicaoPar [x] = 0
somaPosicaoPar (x:y:xs) = y + (somaPosicaoPar xs)

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

-- retorna o ultimo elemento de uma lista
ultimoElemento :: [a] -> a
ultimoElemento [] = error "Nao ha elementos nesta lista"
ultimoElemento [x] = x
ultimoElemento (x:xs) = ultimoElemento xs

-- retorna a lista sem o ultimo elemento
semOUltimo :: [a] -> [a]
semOUltimo [] = []
semOUltimo [a] = []
semOUltimo (x:xs) = x : (semOUltimo xs)

-- shift right
-- shiftr [1,2,3,4]
--  ==> [4,1,2,3]
shiftRight :: [a] -> [a]
shiftRight [] = []
shiftRight xs = (ultimoElemento xs) : (semOUltimo xs)

-- shift right n vezes
shiftRightNVezes :: (Integral b) => [a] -> b -> [a]
shiftRightNVezes xs n
    | n == 1 = (ultimoElemento xs) : (semOUltimo xs)
    | otherwise = shiftRightNVezes ((ultimoElemento xs) : (semOUltimo xs)) (n-1)

-- shift left
-- shiftr [1,2,3,4]
--  ==> [2,3,4,1]
shiftLeft :: [a] -> [a]
shiftLeft [] = []
shiftLeft (x:xs) = xs ++ x:[]

-- shift left n vezes
shiftLeftNVezes :: (Integral b) => [a] -> b -> [a]
shiftLeftNVezes [] _ = []
shiftLeftNVezes (x:xs) n
    | n == 1 = xs ++ x:[]
    | otherwise = shiftLeftNVezes (xs ++ x:[]) (n-1)

-- remove item da lista (1 vez so)
removeItemDaLista :: (Eq a) => [a] -> a -> [a]
removeItemDaLista [] _ = []
removeItemDaLista (x:xs) e
    | x == e = xs
    | otherwise = x : removeItemDaLista xs e

-- remove item da lista (todas as vezes)
removeItemDaLista2 :: (Eq a) => [a] -> a -> [a]
removeItemDaLista2 [] _ = []
removeItemDaLista2 (x:xs) e
    | x /= e = x : removeItemDaLista2 xs e
    | otherwise = removeItemDaLista2 xs e

-- remove item da lista (as primeiras n vezes)
removeItemDaLista3 :: (Eq a, Integral b) => [a] -> a -> b -> [a]
removeItemDaLista3 [] _ _ = []
removeItemDaLista3 (x:xs) e n
    | x == e && n == 1 = xs
    | x == e && n > 1 = removeItemDaLista3 xs e (n-1)
    | otherwise = x : removeItemDaLista3 xs e n
    -- | x == e = (if n == 1 then xs else removeItemDaLista3 xs e (n-1))
    -- | otherwise = x : removeItemDaLista3 xs e (n-1)

-- remove item da lista (a ultima vez que ele aparece) **
removeItemDaLista4 :: (Eq a) => [a] -> a -> [a]
removeItemDaLista4 [] _ = []
removeItemDaLista4 xs e = reverterLista (removeItemDaLista (reverterLista xs) e)

-- troca velho por novo na lista (1 so vez)
trocaVelhoPorNovo :: (Eq a) => [a] -> a -> a -> [a]
trocaVelhoPorNovo [] _ _ = []
trocaVelhoPorNovo (x:xs) v n
    | x == v = n : xs
    | otherwise = x : (trocaVelhoPorNovo xs v n)

-- troca velho por novo na lista (todas vezes)
trocaVelhoPorNovo2 :: (Eq a) => [a] -> a -> a -> [a]
trocaVelhoPorNovo2 [] _ _ = []
trocaVelhoPorNovo2 (x:xs) v n
    | x == v = n : (trocaVelhoPorNovo2 xs v n)
    | otherwise = x : (trocaVelhoPorNovo2 xs v n)

-- troca velho por novo na lista n (as primeiras n vezes)
trocaVelhoPorNovo3 :: (Eq a, Integral b) => [a] -> a -> a -> b -> [a]
trocaVelhoPorNovo3 [] _ _ _ = []
trocaVelhoPorNovo3 (x:xs) v n i
    | x == v && i == 1 = n : xs
    | x == v && i > 1 = n : (trocaVelhoPorNovo3 xs v n (i-1))
    | otherwise = x : (trocaVelhoPorNovo3 xs v n i)
