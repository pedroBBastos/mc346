-- criando meus proprios tipos

-- Isso define uma arvore binária de a seja o que for a
data Tree a = Vazia | No a (Tree a) (Tree a) deriving (Eq,Show,Read)

-- acha um item numa arvore de busca binaria
search :: (Eq b, Ord b, Show b) => (Tree b) -> b -> (Tree b)
search Vazia _ = Vazia
search (No item ae ad) itemBusca
    | itemBusca == item = (No item ae ad)
    | itemBusca < item = search ae itemBusca
    | otherwise = search ad itemBusca

-- insere um item numa abb
insert :: (Eq b, Ord b, Show b) => (Tree b) -> b -> (Tree b)
insert Vazia novo = (No novo Vazia Vazia)
insert (No item ae ad) novo
    | novo < item = No item (insert ae novo) ad
    | novo > item = No item ae (insert ad novo)
    | otherwise = (No item ae ad) -- nao repete item

-- verifica se uma arvore é um abb
verificaAbb :: (Eq b, Ord b, Show b) => (Tree b) -> Bool
verificaAbb raiz = verificaOrdem (converteAbbEmListaInfixa raiz)
    where verificaOrdem [a] = True
          verificaOrdem (x:xs)
              | x < head xs = verificaOrdem xs
              | otherwise = False

-- coverte uma abb numa lista em ordem infixa (arvore-esquerda, no, arvore-direita)
converteAbbEmListaInfixa :: (Eq b, Ord b, Show b) => (Tree b) -> [b]
converteAbbEmListaInfixa Vazia = []
converteAbbEmListaInfixa (No item ae ad) = (converteAbbEmListaInfixa ae) ++ item:[] ++ (converteAbbEmListaInfixa ad)

-- converte uma abb numa lista em ordem prefixa (no, ae, ad)
converteAbbEmListaPrefixa :: (Eq b, Ord b, Show b) => (Tree b) -> [b]
converteAbbEmListaPrefixa Vazia = []
converteAbbEmListaPrefixa (No item ae ad) = item:((converteAbbEmListaPrefixa ae) ++ (converteAbbEmListaPrefixa ad))

-- metodo de ajuda para modificar item de um no
modificaItem :: (Eq b, Ord b, Show b) => (Tree b) -> b -> b -> (Tree b)
modificaItem Vazia _ _ = Vazia
modificaItem (No item ae ad) itemBusca novoItem
    | itemBusca == item = (No novoItem ae ad)
    | itemBusca < item = (No item (modificaItem ae itemBusca novoItem) ad)
    | otherwise = (No item ae (modificaItem ad itemBusca novoItem))

-- remove um item de uma abb
-- pensar nos tres casos
--    no a ser excluido eh folha
--    no a ser excluido tem um filho
--    no a ser excluido tem dois filhos

-- calcula a profundidade maxima (nível) de uma abb
calculaNivelArvore :: (Tree b) -> Int
calculaNivelArvore Vazia = -1 -- devolvendo 0 retornaria a altura da arvore
calculaNivelArvore (No _ ae ad)
    | nivelArvoreEsquerda > nivelArvoreDireita = nivelArvoreEsquerda
    | otherwise = nivelArvoreDireita
    where nivelArvoreEsquerda = (calculaNivelArvore ae) + 1
          nivelArvoreDireita = (calculaNivelArvore ad) + 1

-- converte uma lista em uma abb
converteListaEmAbb :: (Ord a, Eq a, Show a) => [a] -> Tree a
converteListaEmAbb lista = insere Vazia lista
    where insere t [] = t
          insere t (x:xs) = insere (insert t x) xs

--------------- parte de scripts de testes

-- let a = No 4 Vazia Vazia
-- let b = insert a 6
-- let c = insert b (-3)
-- let d = insert c 12
-- let e = insert d 18
