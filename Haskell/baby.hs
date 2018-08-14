doubleMe x = x + x

doubleUs x y = x*2 + y*2

meninoMaroto x y = doubleMe x + doubleMe y

funcaoComIf y = if y > 100
                then y
                else y*2

funcApostrofe' g = (if g >= 13 then "sdsd" else "Lalaland")

-- aqui eh forcado que a seja da typeclass Num e Show. Tem que ser os dois pq na segunda definicao precisa se mostrar o elemento como uma String e na terceira definicao a funcao
-- product eh chamada e ela so pode ser chamada para elementos da Num. -> a eh especificado por duas Typeclasses
multiply :: (Num a, Show a) => [a] -> Int -> String
multiply l 0 = "A lista esta vazia meu sr."
multiply l 1 = "A lista soh tem um elemento e ele eh " ++ show (l !! 0)
multiply l n = "O mulltiplicatorio da bagaca toda eh " ++ show (product l)

recMultiply :: (Num a) => [a] -> a
recMultiply [] = 1
recMultiply (e:list) = e * recMultiply list

-- lembrando que os casos mais prioritarios estao de cima para baixo
recFibonacci :: (Integral a) => a -> a
recFibonacci 0 = 0
recFibonacci 1 = 1
recFibonacci n = recFibonacci (n-1) + recFibonacci (n-2)

-- minha implementacao da funcao zip
myZip :: [a] -> [b] -> [(a,b)]
myZip [] ys = [] -- aqui poderia ter feito "myZip [] _ = []", ja que o que vem em _ nao importa
myZip xs [] = []
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys

-- minha implementacao da funcao elem
myElem :: (Eq a) => a -> [a] -> Bool
myElem _ [] = False
myElem e (x:xs)
      | x == e = True
      | otherwise = myElem e xs

-- invertendo os elementos de uma lista
myReverse :: [a] -> [a]
myReverse [] = []
myReverse [e] = [e]
myReverse (e:xs) = myReverse xs ++ e:[]
