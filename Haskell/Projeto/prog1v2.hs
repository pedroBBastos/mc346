import Data.Graph
import Data.Maybe
import qualified Data.Set as Set

data Tripla = Tripla { node :: String, key :: String, adj :: [String]} deriving (Show, Eq)
data Traslado = Traslado { modo :: String, tempo :: Float} deriving (Show, Eq)
data Distancia = Infinita | Distancia Float deriving (Eq)

instance Ord Distancia where
  compare Infinita (Distancia _) = GT
  compare (Distancia _) Infinita = LT
  compare Infinita Infinita = EQ
  compare (Distancia a) (Distancia b) = compare a b

-----------------------------------------

splitAll :: (Eq a) => a -> [a] -> [[a]]
splitAll elemento lista = splitAll' elemento lista []
    where splitAll' e [] acc = [acc]
          splitAll' e (x:xs) acc
              | x == e = acc:(splitAll' e xs [])
              | otherwise = splitAll' e xs (acc++x:[])

paraTriplaConvencional :: [Tripla] -> [(String, String, [String])]
paraTriplaConvencional vTripla = foldl (\acc t -> acc ++ [(node t, key t, adj t)]) [] vTripla

periodosOnibus :: [String] -> [(String, Float)]
periodosOnibus v = foldl (\acc p -> let [linha, tempoEspera] = splitAll ' ' p in acc ++ [(linha, read tempoEspera)]) [] v

criaTriplas :: [String] -> [Tripla]
criaTriplas listaAdjacencias = foldl (\acc x -> add (splitAll ' ' x) acc) [] listaAdjacencias
    where add (origem:destino:resto) vTripla =
            let vTripla' = if pegaVertice origem vTripla == Nothing then vTripla ++ [Tripla {node=origem, key=origem, adj=[]}] else vTripla
                vTripla'' = if pegaVertice destino vTripla == Nothing then vTripla' ++ [Tripla {node=destino, key=destino, adj=[]}] else vTripla'
                vTripla''' = adicionaAresta vTripla''
            in vTripla'''
            where adicionaAresta (t:ts)
                    | origem == node t =
                      if destino `elem` (adj t)
                        then t:ts
                        else Tripla {node=origem, key=origem, adj=((adj t)++[destino])} : ts
                    | otherwise = t : adicionaAresta ts
          pegaVertice c vTripla = foldl (\acc t -> if node t == c then Just t else acc) Nothing vTripla

-- listaAdjacencias, periodosOnibus
criaTabelaDeCaminhos :: [String] -> [(String, Float)] -> (String -> Maybe Vertex) -> [(Edge, [Traslado])]
criaTabelaDeCaminhos listaAdjacencias po vertexFromKey = foldl (\acc x -> atualizaCaminhos x acc) [] listaAdjacencias
    where atualizaCaminhos adj tabela =
            let [o, d, oModo, oTempo] = splitAll ' ' adj
                theEdge = (fromJust $ vertexFromKey o, fromJust $ vertexFromKey d)
                timeExpend = read oTempo + (waitTime oModo)/2
            in updateTable theEdge oModo timeExpend tabela
            where waitTime linha = foldl (\acc x -> if fst x == linha then snd x else acc) 0 po
                  updateTable e m t [] = [(e, [Traslado {modo=m, tempo=t}])]
                  updateTable e m t (t1:resto)
                      | e == fst t1 = (e, snd t1 ++ [Traslado {modo=m, tempo=t}]) : resto
                      | otherwise = t1 : updateTable e m t resto

-- criaTabelaDeCaminhos' :: [String] -> [(String, Float)] -> (String -> Maybe Vertex) -> [Int]
-- criaTabelaDeCaminhos' listaAdjacencias po vertexFromKey = foldl (\acc x -> acc ++ [atualizaCaminhos x]) [] listaAdjacencias
--     where atualizaCaminhos adj = length (splitAll ' ' adj)

constroiFilaDePrioridade :: [Vertex] -> Vertex -> (Set.Set (Distancia, Vertex))
constroiFilaDePrioridade listaVertices vOrigem = foldl (\acc v -> if v == vOrigem then Set.insert (Distancia 0.0, v) acc else Set.insert (Infinita, v) acc) (Set.fromList []) listaVertices

-- (graph, nodeFromVertex, vertexFromKey), tabelaAdjacencias, origem, destino, saidaPlanejada
dijkstra :: (Graph, (Vertex -> (String, String, [String])), (String -> Maybe Vertex)) -> [(Edge, [Traslado])] -> String -> String -> String
dijkstra (graph, nodeFromVertex, vertexFromKey) tabelaAdjacencias o d =
    let filaP = constroiFilaDePrioridade (vertices graph) (vertexFromKey o)
    in rodaDijkstra filaP tabelaAdjacencias
    where rodaDijkstra (graph, nodeFromVertex, vertexFromKey) f t d v acc -- (graph, nodeFromVertex, vertexFromKey), filaPrioridade, tabelaAdjacencias, destino, verticeAtual, acc
            | elemAt 0 f == d = d : acc
            | otherwise = let (label, key, adjVAtual) = nodeFromVertex v
                              subsetAdj = (\a -> foldl (\acc e -> acc ++ (filter ((==(v, vertexFromKey e)).fst) t)) [] a) adjVAtual -- quero um subset da listaAdjacencias que seja referente as adjacencias do no atual
                              f' = (\fp l -> foldl (\acc e -> Set.insert (Distancia menorTraslado $ snd e, _______________________) (Set.delete ___ acc) ) fp l) f subsetAdj -- atualizacao da fila de prioridade
                          in asdasd -- chamar recursao aqui
                          where menorTraslado [] = Nothing
                                menorTraslado traslados = foldl (\acc t -> if tempo t < tempo $ fromJust acc then Just t else acc) (Just $ head traslados) traslados

process :: String -> String
process input =
  let allLines = lines input
      [listaAdjacencias, pOnibus, desiredWay] = splitAll "" allLines
      h = paraTriplaConvencional $ criaTriplas listaAdjacencias
      (graph, nodeFromVertex, vertexFromKey) = graphFromEdges h -- criacao do grafo a partir das triplas criadas a partir da entrada
      tabelaAdjacencias = criaTabelaDeCaminhos listaAdjacencias (periodosOnibus pOnibus) vertexFromKey
      --result = show graph
      result = show tabelaAdjacencias
      --result = show h
      --result = show listaAdjacencias
  in result

-----------------------------------------

main = interact process
