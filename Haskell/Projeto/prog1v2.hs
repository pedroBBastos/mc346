import Data.Graph
import Data.Maybe
import qualified Data.Set as Set

data Tripla = Tripla { node :: String, key :: String, adj :: [String]} deriving (Show, Eq)
data Traslado = Traslado { modo :: String, tempo :: Float} deriving (Show, Eq)
data Distancia = Infinita | Distancia { valorDistancia :: Float} deriving (Eq)
data ItemFilaPrioridade = ItemFilaPrioridade {distanciaAteAqui :: Distancia, verticeAtual :: Vertex, verticeAnterior :: (Maybe Vertex, Maybe String)} deriving (Show, Eq)

instance Ord Distancia where
  compare Infinita (Distancia _) = GT
  compare (Distancia _) Infinita = LT
  compare Infinita Infinita = EQ
  compare (Distancia a) (Distancia b) = compare a b

instance Ord ItemFilaPrioridade where
  compare a b = if comparaDistancias == EQ then compare (verticeAtual a) (verticeAtual b) else comparaDistancias
      where comparaDistancias = compare (distanciaAteAqui a) (distanciaAteAqui b)

instance Show Distancia where
  show Infinita = "Infinita"
  show (Distancia d) = show d

-----------------------------------------

processaEntrada :: [String] -> [[String]]
processaEntrada [] = []
processaEntrada entrada = let e = takeWhile (/= "") entrada
                              r = dropWhile (/= "") entrada
                              r' = if length r == 0 then r else tail r
                          in [e] ++ processaEntrada r'

paraTriplaConvencional :: [Tripla] -> [(String, String, [String])]
paraTriplaConvencional vTripla = foldl (\acc t -> acc ++ [(node t, key t, adj t)]) [] vTripla

periodosOnibus :: [String] -> [(String, Float)]
periodosOnibus v = foldl (\acc p -> let [linha, tempoEspera] = words p in acc ++ [(linha, read tempoEspera)]) [] v

criaTriplas :: [String] -> [Tripla]
criaTriplas listaAdjacencias = foldl (\acc x -> add (words x) acc) [] listaAdjacencias
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
            let [o, d, oModo, oTempo] = words adj
                theEdge = (fromJust $ vertexFromKey o, fromJust $ vertexFromKey d)
                timeExpend = read oTempo + (waitTime oModo)/2
            in updateTable theEdge oModo timeExpend tabela
            where waitTime linha = foldl (\acc x -> if fst x == linha then snd x else acc) 0 po
                  updateTable e m t [] = [(e, [Traslado {modo=m, tempo=t}])]
                  updateTable e m t (t1:resto)
                      | e == fst t1 = (e, snd t1 ++ [Traslado {modo=m, tempo=t}]) : resto
                      | otherwise = t1 : updateTable e m t resto

constroiFilaDePrioridade :: [Vertex] -> Vertex -> (Set.Set ItemFilaPrioridade)
constroiFilaDePrioridade listaVertices vOrigem = foldl (\acc v -> if v == vOrigem
                                                                    then Set.insert (novoItemFilaPrioridade (Distancia 0.0) v (Nothing, Nothing)) acc
                                                                    else Set.insert (novoItemFilaPrioridade Infinita v (Nothing, Nothing)) acc) (Set.fromList []) listaVertices

encontraItemPorVertice :: Vertex -> (Set.Set ItemFilaPrioridade) -> Maybe ItemFilaPrioridade
encontraItemPorVertice ve set = Set.foldl' (\acc x -> if verticeAtual x == ve then Just x else acc) Nothing set

novoItemFilaPrioridade :: Distancia -> Vertex -> (Maybe Vertex, Maybe String) -> ItemFilaPrioridade
novoItemFilaPrioridade d ve va = ItemFilaPrioridade {distanciaAteAqui=d, verticeAtual=ve, verticeAnterior=va}

removeItemFilaPrioridade :: (Set.Set ItemFilaPrioridade) -> Maybe ItemFilaPrioridade -> (Set.Set ItemFilaPrioridade)
removeItemFilaPrioridade set item = if item == Nothing then set else Set.delete (fromJust item) set

-- dijkstra :: (Graph, (Vertex -> (String, String, [String])), (String -> Maybe Vertex)) -> [(Edge, [Traslado])] -> Vertex -> Vertex -> [ItemFilaPrioridade]
-- dijkstra (graph, nodeFromVertex, vertexFromKey) tabelaAdjacencias origem destino =
--     let filaP = constroiFilaDePrioridade (vertices graph) origem
--     in rodaDijkstra filaP origem []
--     where rodaDijkstra f v c -- filaPrioridade, verticeAtual, visitados
--             | v == destino = [Set.elemAt 0 f]
--             | otherwise = let (label, key, adjVAtual) = nodeFromVertex v
--                               subsetAdj = (\a -> foldl (\acc e -> acc ++ (filter ((==(v, fromJust $ vertexFromKey e)).fst) tabelaAdjacencias)) [] a) adjVAtual -- quero um subset da listaAdjacencias que seja referente as adjacencias do no atual
--                               subsetAdj' = foldl (\acc e -> if (snd $ fst e) `elem` c then acc else acc ++ [e]) [] subsetAdj -- remover de subsetAdj os vertices que ja foram visitados
--                               topo = Set.elemAt 0 f
--                               f' = (\fp l -> foldl (\acc e -> Set.insert (novoItemFilaPrioridade (Distancia $ (tempo $ fromJust $ menorTraslado $ snd e) + (valorDistancia $ distanciaAteAqui topo)) (snd $ fst e) (Just v, Just $ modo $ fromJust $ menorTraslado $ snd e)) (removeItemFilaPrioridade acc (encontraItemPorVertice (snd $ fst e) acc)) ) fp l) f subsetAdj' -- atualizacao da fila de prioridade
--                               f'' = Set.deleteAt 0 f'
--                           in topo : (rodaDijkstra f'' (verticeAtual $ Set.elemAt 0 f'') (c ++ [v])) -- chamar recursao aqui
--                           where menorTraslado [] = Nothing
--                                 menorTraslado traslados = foldl (\acc t -> if acc == Nothing || tempo t < tempo (fromJust acc) then Just t else acc) Nothing traslados

dijkstra :: (Graph, (Vertex -> (String, String, [String])), (String -> Maybe Vertex)) -> [(Edge, [Traslado])] -> Vertex -> Vertex -> [Set.Set ItemFilaPrioridade]
dijkstra (graph, nodeFromVertex, vertexFromKey) tabelaAdjacencias origem destino =
    let filaP = constroiFilaDePrioridade (vertices graph) origem
    in rodaDijkstra filaP origem []
    where rodaDijkstra f v c -- filaPrioridade, verticeAtual, visitados
            -- | v == destino = [Set.elemAt 0 f]
            | v == destino = [f]
            | otherwise = let (label, key, adjVAtual) = nodeFromVertex v
                              subsetAdj = (\a -> foldl (\acc e -> acc ++ (filter ((==(v, fromJust $ vertexFromKey e)).fst) tabelaAdjacencias)) [] a) adjVAtual -- quero um subset da listaAdjacencias que seja referente as adjacencias do no atual
                              subsetAdj' = foldl (\acc e -> if (snd $ fst e) `elem` c then acc else acc ++ [e]) [] subsetAdj -- remover de subsetAdj os vertices que ja foram visitados
                              topo = Set.elemAt 0 f
                              f' = (\fp l -> foldl (\acc e -> Set.insert (novoItemFilaPrioridade (Distancia $ (tempo $ fromJust $ menorTraslado $ snd e) + (valorDistancia $ distanciaAteAqui topo)) (snd $ fst e) (Just v, Just $ modo $ fromJust $ menorTraslado $ snd e)) (removeItemFilaPrioridade acc (encontraItemPorVertice (snd $ fst e) acc)) ) fp l) f subsetAdj' -- atualizacao da fila de prioridade
                              f'' = Set.deleteAt 0 f'
                          in f : f' : f'' : (rodaDijkstra f'' (verticeAtual $ Set.elemAt 0 f'') (c ++ [v])) -- chamar recursao aqui
                          where menorTraslado [] = Nothing
                                menorTraslado traslados = foldl (\acc t -> if acc == Nothing || tempo t < tempo (fromJust acc) then Just t else acc) Nothing traslados
                                verificaAtualizacaoDistancia atual nova = if nova < atual then nova else atual



constroiFilaDePrioridade2 :: [Vertex] -> Vertex -> [ItemFilaPrioridade]
constroiFilaDePrioridade2 listaVertices vOrigem = sort $ foldl (\acc v -> if v == vOrigem
                                                                            then acc ++ [novoItemFilaPrioridade (Distancia 0.0) v (Nothing, Nothing)] acc
                                                                            else acc ++ [novoItemFilaPrioridade Infinita v (Nothing, Nothing)] acc) [] listaVertices

dijkstra2 :: (Graph, (Vertex -> (String, String, [String])), (String -> Maybe Vertex)) -> [(Edge, [Traslado])] -> Vertex -> Vertex -> [[ItemFilaPrioridade]]
dijkstra2 (graph, nodeFromVertex, vertexFromKey) tabelaAdjacencias origem destino = rodaDijkstra (constroiFilaDePrioridade2 (vertices graph) origem) origem [] -- passando filaPrioridade, verticeAtual e visitados
    where rodaDijkstra f v c =
      let (label, key, adjVAtual) = nodeFromVertex v
          subsetAdj = (\a -> foldl (\acc e -> acc ++ (filter ((==(v, fromJust $ vertexFromKey e)).fst) tabelaAdjacencias)) [] a) adjVAtual -- quero um subset da listaAdjacencias que seja referente as adjacencias do no atual
          subsetAdj' = foldl (\acc e -> if (snd $ fst e) `elem` c then acc else acc ++ [e]) [] subsetAdj -- remover de subsetAdj os vertices que ja foram visitados
          topo = head f
          f' = sort $ (\fp l -> foldl (\acc e -> atualizaItemFilaPrioridade e fp d) fp l) f subsetAdj' -- atualizacao da fila de prioridade
      in rodaDijkstra f' (verticeAtual )

-- (Distancia $ (tempo $ fromJust $ menorTraslado $ snd e) + (valorDistancia $ distanciaAteAqui topo))
atualizaItemFilaPrioridade :: (Edge, [Traslado]) -> [ItemFilaPrioridade] -> Distancia -> [ItemFilaPrioridade]
atualizaItemFilaPrioridade adj fp = let verticeAdjacencia = snd $ fst adj
                                        vAnterior = fst $ fst adj
                                        melhorCaminho = menorTraslado $ snd adj
                                    in foldl (\acc e -> if verticeAtual e == verticeAdjacencia then acc ++ [novoItemFilaPrioridade () verticeAdjacencia (vAnterior, )] else acc ++ [e]) [] fp
                                    where menorTraslado [] = Nothing
                                          menorTraslado traslados = foldl (\acc t -> if acc == Nothing || tempo t < tempo (fromJust acc) then Just t else acc) Nothing traslados
                                          novaDistancia



















saida :: [ItemFilaPrioridade] -> (Vertex -> (String, String, [String])) -> String
saida l nodeFromVertex = tail $ fst $ foldr (\i acc -> if snd acc == Nothing || (fromJust $ snd acc)  == verticeAtual i
                                                        then ((fromMaybe "" (snd $ verticeAnterior i)) ++ " " ++ (pegaIdVertice $ verticeAtual i) ++ " " ++ fst acc, fst $ verticeAnterior i)
                                                        else acc) ("", Nothing) l
    where pegaIdVertice v = let (label, _, _) = nodeFromVertex v in label

process :: String -> String
process input =
  let allLines = lines input
      [listaAdjacencias, pOnibus, [desiredWay]] = processaEntrada allLines
      (graph, nodeFromVertex, vertexFromKey) = graphFromEdges $ paraTriplaConvencional $ criaTriplas listaAdjacencias -- criacao do grafo a partir das triplas criadas a partir da entrada
      tabelaAdjacencias = criaTabelaDeCaminhos listaAdjacencias (periodosOnibus pOnibus) vertexFromKey
      --result = show graph
      --result = show tabelaAdjacencias
      --result = show h
      --result = show listaAdjacencias
      [o, d] = words desiredWay
      result = show $ dijkstra (graph, nodeFromVertex, vertexFromKey) tabelaAdjacencias (fromJust $ vertexFromKey o) (fromJust $ vertexFromKey d)
      --dijkstraResult = dijkstra (graph, nodeFromVertex, vertexFromKey) tabelaAdjacencias (fromJust $ vertexFromKey o) (fromJust $ vertexFromKey d)
      --result = (saida dijkstraResult nodeFromVertex) ++ "\n"  ++ (show $ distanciaAteAqui $ last dijkstraResult) ++ "\n"
      --(label, key, adjVAtual) = nodeFromVertex 3
      --result = show $ verificaSubAdj tabelaAdjacencias vertexFromKey 3 adjVAtual
  in result

-----------------------------------------

main = interact process
