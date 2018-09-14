import Data.Char

data Grafo = Grafo [Vertice] -- estrutura principal que representa um grafo
data Vertice = Vertice Char [Aresta] -- identificador do vertice e a lista de adjacencias
data Aresta = Aresta Vertice Modo Float -- o modo e o tempo que leva para percorrer a aresta
data Modo = Pe | Onibus Float -- a pé nao tem período e o tempo será dado no tipo aresta.
                              -- Ja onibus tem um período de saida, que deverá ser levado em conta

-----------------------------------------
-- FUNCOES AUXILIARES

splitAll :: (Eq a) => a -> [a] -> [[a]]
splitAll elemento lista = splitAll' elemento lista []
    where splitAll' e [] acc = [acc]
          splitAll' e (x:xs) acc
              | x == e = acc:(splitAll' e xs [])
              | otherwise = splitAll' e xs (acc++x:[])

splitEntryData :: [String] -> [String] -> [[String]]
splitEntryData [] acc = [acc]
splitEntryData (line:rest) acc
    | line == "" = acc : (splitEntryData rest [])
    | otherwise = splitEntryData rest (line : acc)

-- createGraph :: [String] -> Grafo -- aqui tratar a primeira parte da entrada (criacao do grafo)

-- signNonFootWays -- aqui tratar segunda parte da entrada (periodo de saida de onibus e metros)

-- calculateBestWay -- rodar o dijkstra a parte da terceira parte da entrada (saida e destino)

process :: String -> String
process input =
  let allLines = lines input
      ab = splitEntryData allLines []
      result = show ab
  in result

-----------------------------------------

main = interact process
