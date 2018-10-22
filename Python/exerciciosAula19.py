# pares: dado um iterator, retorna um iterator com os elementos nas posicoes pares (0,2)
# Nessa forma, quando a lista tem numero impar de elementos, quando tenta atribuir algo para y,
# eh lancada a excecao StopIteration. Como este generator esta sendo usado em um for, o for ira parar,
# sem retornar o ultimo valor que deveria (o atribuido para a variavel x)
def pares(it):
    while True:
        x = it.__next__()
        y = it.__next__()
        yield x

def pares2(it):
    while True:
        try:
            x = it.__next__()
        except StopIteration as e: # se nao ha mais elementos, excecao eh lancada para parar iteracao
            raise e
        try:
            y = it.__next__()
        except StopIteration as e: # se lancou excecao aqui, foi atribuido valor a x e tenho que devolve-lo
            pass
        yield x

# reverte: dado um iterator, reverte ele *, retornando um iterator
def reverte(it):
    aux = []
    for e in it:
        aux.append(e)
    aux.reverse()
    for out in aux:
        yield out

# zip: dado 2 iterators, retorna um iterator que retorna os elementos intercalados
def zip(it1, it2):
    while True:
        x = it1.__next__()
        yield x
        y = it2.__next__()
        yield y

def zip2(it1, it2):
    acabou1 = False
    acabou2 = False
    while  True:
        try:
            x = it1.__next__()
            yield x
        except StopIteration as e:
            if acabou2:
                raise e
            else:
                acabou1 = True
                pass
        try:
            y = it2.__next__()
            yield y
        except StopIteration as e:
            if acabou1:
                raise e
            else:
                acabou2 = True
                pass

# cart: dado 2 iterators, retorna um iterator com o produto cartesiano dos elementos *
def prodCartesiano(it1, it2):
    items1 = []
    items2 = []
    aux = []
    for i in it1:
        items1.append(i)
    for e in it2:
        items2.append(e)
    for i in items1:
        for e in items2:
            aux.append((i, e))
    for p in aux:
        yield p

def prodCartesiano2(it1, it2):
    items2 = []
    for e in it2:
        items2.append(e)
    while True:
        i = it1.__next__() # aqui, como mexo diretamente com o iterator, nao perco tempo de execucao
        for e in items2: # fazer esse for nao abre o iterator, o que pode gerar maior tempo de execucao
            yield (i, e)

# ciclo: dado um iterator, retorna os elemento snum ciclo infinito
def ciclo(it):
    items = []
    consumed = False
    while True:
        if consumed:
            it = iter(items)
            items = []
            consumed = False
        else:
            try:
                x = it.__next__()
                items.append(x)
                yield x
            except StopIteration as e:
                consumed = True
                pass

# rangeinf(init,passo=1): retorna um iterator que gera numeros de init ate infinito, com passo
def rangeinf(init,passo=1):
    while True:
        yield init
        init += passo

# take: como o take do haskell
def take(n, itLista):
    result = []
    while n>0:
        try:
            result.append(itLista.__next__())
            n -= 1
        except StopIteration as e:
            return result
    return result

# drop - como o drop do haskell
def drop(n, itLista):
    result = []
    while n>0:
        try:
            itLista.__next__()
            n -= 1
        except StopIteration as e:
            break
    while True:
        try:
            result.append(itLista.__next__())
        except StopIteration as e:
            return result
    return result
