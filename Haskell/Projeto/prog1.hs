import Data.Char
import Data.Maybe

data Grafo = Grafo [Vertice] deriving (Show)-- estrutura principal que representa um grafo
data Vertice = Vertice String [Aresta] deriving (Eq, Show) -- identificador do vertice e a lista de adjacencias
data Aresta = Aresta String Modo Float deriving (Eq, Show) -- o modo e o tempo que leva para percorrer a aresta
data Modo = Pe | Onibus String deriving (Eq, Show) -- a pé nao tem período e o tempo será dado no tipo aresta.
                               -- Ja onibus tem um período de saida, que deverá ser levado em conta

-----------------------------------------
-- FUNCOES AUXILIARES

splitAll :: (Eq a) => a -> [a] -> [[a]]
splitAll elemento lista = splitAll' elemento lista []
    where splitAll' e [] acc = [acc]
          splitAll' e (x:xs) acc
              | x == e = acc:(splitAll' e xs [])
              | otherwise = splitAll' e xs (acc++x:[])

getModo :: String -> Modo
getModo "a-pe" = Pe
getModo s = (Onibus s)

createGraph :: [[String]] -> Grafo -- aqui tratar a primeira parte da entrada (criacao do grafo)
createGraph (listaAdjacencias:resto) = createGraph' listaAdjacencias (Grafo [])
    where createGraph' [] g = g
          createGraph' (a:as) (Grafo v) = createGraph' as (Grafo (insereNovaAdjacencia v $ splitAll ' ' a))

insereNovaAdjacencia :: [Vertice] -> [String] -> [Vertice]
insereNovaAdjacencia v (origem:destino:modo:tempo:resto) =
    let vOrigem = pegaVertice origem v
        vDestino = pegaVertice destino v
        v' = if vOrigem == Nothing then v ++ [(Vertice origem [])] else v
        v'' = if vDestino == Nothing then v' ++ [(Vertice destino [])] else v'
        v''' = criaAresta v''
    in v'''
    where pegaVertice c vs = foldl (\acc (Vertice idVertice a) -> if idVertice == c then Just (Vertice idVertice a) else acc) Nothing vs
          criaAresta ((Vertice idVertice a):ves)
            | origem == idVertice = (Vertice idVertice (a++[(Aresta destino (getModo modo) (read tempo))])):ves
            | otherwise = (Vertice idVertice a) : (criaAresta ves)

-- signNonFootWays -- aqui tratar segunda parte da entrada (periodo de saida de onibus e metros)

-- calculateBestWay -- rodar o dijkstra a parte da terceira parte da entrada (saida e destino)

process :: String -> String
process input =
  let allLines = lines input
      ab = splitAll "" allLines
      h = createGraph ab
      result = show h
  in result

-----------------------------------------

main = interact process
