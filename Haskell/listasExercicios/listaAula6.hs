-- soma dos elementos nas posições pares da lista ( o primeiro elemento esta na posicao 1)
soma :: (Num b) => [b] -> b
soma l = foldl (+) 0 (pegaPosicoesPares l)

pegaPosicoesPares :: (Num b) => [b] -> [b]
pegaPosicoesPares [] = []
pegaPosicoesPares [a] = []
pegaPosicoesPares (x:y:xs) = [y] ++ (pegaPosicoesPares xs)
