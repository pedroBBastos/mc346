-- Pedro Barros Bastos      RA : 204481
-- Rafael Ferreira Galib    RA : 204904

import Data.Graph -- estrutura padrao que eh usada para representar um grafo
import Data.Maybe
import qualified Data.Set as Set -- -- estrutura padrao que eh usada como uma fila de prioridade

-- Tripla : tipo criado para melhor se trabalhar com triplas. Usado para criacao das adjacencias.
data Tripla = Tripla { node :: String, key :: String, adj :: [String]} deriving (Show, Eq)
-- Traslado : tipo para representar um modo de caminho e o tempo deste.
data Traslado = Traslado { modo :: String, tempo :: Float} deriving (Show, Eq)
-- Distancia : tipo para representar distancia no grafo.
data Distancia = Infinita | Distancia { valorDistancia :: Float} deriving (Eq)
-- ItemFilaPrioridade : tipo que representa um item na fila de prioridade que eh usada durante o dijkstra.
data ItemFilaPrioridade = ItemFilaPrioridade {distanciaAteAqui :: Distancia, verticeAtual :: Vertex, verticeAnterior :: (Maybe Vertex, Maybe String)} deriving (Show, Eq)

instance Ord Distancia where -- implementando a forma de ordenacao entre tipos Distancia
  compare Infinita (Distancia _) = GT
  compare (Distancia _) Infinita = LT
  compare Infinita Infinita = EQ
  compare (Distancia a) (Distancia b) = compare a b

instance Ord ItemFilaPrioridade where -- implementando a forma de ordenacao entre tipos ItemFilaPrioridade
  compare a b = if comparaDistancias == EQ then compare (verticeAtual a) (verticeAtual b) else comparaDistancias
      where comparaDistancias = compare (distanciaAteAqui a) (distanciaAteAqui b)

instance Show Distancia where -- implementando a forma de exibicao entre tipos Distancia
  show Infinita = "Infinita"
  show (Distancia d) = show d

-----------------------------------------

main = interact process -- main

-----------------------------------------

-- funcao criada para processar entrada e retornar uma saida.
process :: String -> String
process input =
  let allLines = lines input
      [listaAdjacencias, pOnibus, [desiredWay]] = processaEntrada allLines
      (graph, nodeFromVertex, vertexFromKey) = graphFromEdges $ paraTriplaConvencional $ criaTriplas listaAdjacencias -- criacao do grafo a partir das triplas criadas a partir da entrada
      tabelaAdjacencias = criaTabelaDeCaminhos listaAdjacencias vertexFromKey
      [o, d] = words desiredWay
      dijkstraResult = dijkstra (graph, nodeFromVertex, vertexFromKey) tabelaAdjacencias (periodosOnibus pOnibus) (fromJust $ vertexFromKey o) (fromJust $ vertexFromKey d)
      result = (saida dijkstraResult nodeFromVertex) ++ "\n"  ++ (show $ distanciaAteAqui $ last dijkstraResult) ++ "\n"
  in result

-- processa a entrada e retorna uma lista de lista de string.
processaEntrada :: [String] -> [[String]]
processaEntrada [] = []
processaEntrada entrada = let e = takeWhile (/= "") entrada
                              r = dropWhile (/= "") entrada
                              r' = if length r == 0 then r else tail r
                          in [e] ++ processaEntrada r'

-- cria triplas correspondentes a primeira parte da entrada, as adjacencias e seus pesos.
criaTriplas :: [String] -> [Tripla]
criaTriplas listaAdjacencias = foldl (\acc x -> add (take 2 $ words x) acc) [] listaAdjacencias
    where add lAdj@[origem, destino] vTripla = adicionaAresta (foldl (\acc x -> if pegaVertice x acc == Nothing
                                                                                  then acc ++ [Tripla {node=x, key=x, adj=[]}]
                                                                                  else acc) vTripla lAdj)
            where adicionaAresta (t:ts)
                    | origem == node t =
                      if destino `elem` (adj t)
                        then t:ts
                        else Tripla {node=origem, key=origem, adj=((adj t)++[destino])} : ts
                    | otherwise = t : adicionaAresta ts
          pegaVertice c vTripla = foldl (\acc t -> if node t == c then Just t else acc) Nothing vTripla

-- funcao que converte uma lista do tipo Tripla para uma lista de triplas padrao.
paraTriplaConvencional :: [Tripla] -> [(String, String, [String])]
paraTriplaConvencional vTripla = foldl (\acc t -> acc ++ [(node t, key t, adj t)]) [] vTripla

-- cria um vetor de tuplas que relaciona cada aresta do grafo com seus possiveis caminhos
criaTabelaDeCaminhos :: [String] -> (String -> Maybe Vertex) -> [(Edge, [Traslado])]
criaTabelaDeCaminhos listaAdjacencias vertexFromKey = foldl (\acc x -> atualizaCaminhos x acc) [] listaAdjacencias
    where atualizaCaminhos adj tabela =
            let [o, d, oModo, oTempo] = words adj
                theEdge = (fromJust $ vertexFromKey o, fromJust $ vertexFromKey d)
                timeExpend = read oTempo
            in updateTable theEdge oModo timeExpend tabela
            where updateTable e m t [] = [(e, [Traslado {modo=m, tempo=t}])]
                  updateTable e m t (t1:resto)
                      | e == fst t1 = (e, snd t1 ++ [Traslado {modo=m, tempo=t}]) : resto
                      | otherwise = t1 : updateTable e m t resto

-- funcao que roda o dijkstra a partir do grafo criado.
dijkstra :: (Graph, (Vertex -> (String, String, [String])), (String -> Maybe Vertex)) -> [(Edge, [Traslado])] -> [(String, Float)] -> Vertex -> Vertex -> [ItemFilaPrioridade]
dijkstra (graph, nodeFromVertex, vertexFromKey) tabelaAdjacencias periodoOnibus origem destino =
    rodaDijkstra (constroiFilaDePrioridade (vertices graph) origem) origem []
    where rodaDijkstra f v c -- filaPrioridade, verticeAtual, visitados
            | v == destino = [Set.elemAt 0 f]
            | otherwise = let (label, key, adjVAtual) = nodeFromVertex v
                              subsetAdj = (\a -> foldl (\acc e -> acc ++ (filter ((==(v, fromJust $ vertexFromKey e)).fst) tabelaAdjacencias)) [] a) adjVAtual -- quero um subset da listaAdjacencias que seja referente as adjacencias do no atual
                              subsetAdj' = foldl (\acc e -> if (snd $ fst e) `elem` c then acc else acc ++ [e]) [] subsetAdj -- remover de subsetAdj os vertices que ja foram visitados
                              topo = Set.elemAt 0 f
                              f' = atualizaFilaPrioridade f topo subsetAdj' periodoOnibus
                              f'' = Set.deleteAt 0 f'
                          in topo : (rodaDijkstra f'' (verticeAtual $ Set.elemAt 0 f'') (c ++ [v]))

-- cria um vetor de tuplas para relacionar as linhas de onibus com seus respectivos periodos de saida.
periodosOnibus :: [String] -> [(String, Float)]
periodosOnibus v = foldl (\acc p -> let [linha, tempoEspera] = words p in acc ++ [(linha, read tempoEspera)]) [] v

-- dado o retorno do djikstra, monta a string de saida para exibir o menor caminho calculado.
saida :: [ItemFilaPrioridade] -> (Vertex -> (String, String, [String])) -> String
saida l nodeFromVertex = tail $ fst $ foldr (\i acc -> if snd acc == Nothing || (fromJust $ snd acc)  == verticeAtual i
                                                        then ((fromMaybe "" (snd $ verticeAnterior i)) ++ " " ++ (pegaIdVertice $ verticeAtual i) ++ " " ++ fst acc, fst $ verticeAnterior i)
                                                        else acc) ("", Nothing) l
    where pegaIdVertice v = let (label, _, _) = nodeFromVertex v in label

-- as funcoes abaixo operam sobre a fila de prioridade, seja para manipula-la ou para criar um novo item, etc

novoItemFilaPrioridade :: Distancia -> Vertex -> (Maybe Vertex, Maybe String) -> ItemFilaPrioridade
novoItemFilaPrioridade d ve va = ItemFilaPrioridade {distanciaAteAqui=d, verticeAtual=ve, verticeAnterior=va}

removeItemFilaPrioridade :: (Set.Set ItemFilaPrioridade) -> Maybe ItemFilaPrioridade -> (Set.Set ItemFilaPrioridade)
removeItemFilaPrioridade set item = if item == Nothing then set else Set.delete (fromJust item) set

encontraItemPorVertice :: Vertex -> (Set.Set ItemFilaPrioridade) -> Maybe ItemFilaPrioridade
encontraItemPorVertice ve set = Set.foldl' (\acc x -> if verticeAtual x == ve then Just x else acc) Nothing set

constroiFilaDePrioridade :: [Vertex] -> Vertex -> (Set.Set ItemFilaPrioridade)
constroiFilaDePrioridade listaVertices vOrigem = foldl (\acc v -> if v == vOrigem
                                                                    then Set.insert (novoItemFilaPrioridade (Distancia 0.0) v (Nothing, Nothing)) acc
                                                                    else Set.insert (novoItemFilaPrioridade Infinita v (Nothing, Nothing)) acc) (Set.fromList []) listaVertices

-- onde o efetivo calculo do menor caminho eh feito a partir da lista adjacencias de um dado vertice
atualizaFilaPrioridade :: (Set.Set ItemFilaPrioridade) -> ItemFilaPrioridade -> [(Edge, [Traslado])] -> [(String, Float)] -> (Set.Set ItemFilaPrioridade)
atualizaFilaPrioridade fp atual adjs po = foldl (\acc e -> let item = encontraItemPorVertice (snd $ fst e) acc
                                                               melhorTraslado = fromJust $ menorTraslado $ snd e
                                                               novaDistancia = distanciaAtualizada melhorTraslado (snd $ verticeAnterior atual)
                                                           in if novaDistancia < (distanciaAteAqui $ fromJust item)
                                                              then Set.insert
                                                                  (novoItemFilaPrioridade novaDistancia (snd $ fst e) (Just (fst $ fst e), Just $ modo melhorTraslado))
                                                                  (removeItemFilaPrioridade acc item)
                                                              else acc) fp adjs
    where distanciaAtualizada mt Nothing = Distancia $ (tempo mt) + (valorDistancia $ distanciaAteAqui atual) + ((waitTime (modo mt))/2)
          distanciaAtualizada mt ultimoModo = if (fromJust ultimoModo) /= (modo mt)
                                                then Distancia $ (tempo mt) + (valorDistancia $ distanciaAteAqui atual) + ((waitTime (modo mt))/2)
                                                else Distancia $ (tempo mt) + (valorDistancia $ distanciaAteAqui atual)
          menorTraslado [] = Nothing
          menorTraslado traslados = foldl (\acc t -> if acc == Nothing || tempo t < tempo (fromJust acc) then Just t else acc) Nothing traslados
          waitTime linha = foldl (\acc x -> if fst x == linha then snd x else acc) 0 po
