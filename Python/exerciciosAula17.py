# criar uma lista com apenas os valores pares de outra lista
def valoresPares(l):
    return [x for x in l if x % 2 == 0]

# criar uma lista com os valores nas posicoes pares
def valoresPosicoesPares(l):
    r = []
    for i in range(2,len(l)+1,2):
         r.append(l[i-1])
    return r

# criar um dicionario com a contagem de cada elemento numa lista
def contaElementos(l):
    d = {}
    for e in l:
        if e in d:
            d[e] += 1
        else:
            d[e] = 1
    return d

# qual é a chave associada ao maior valore num dicionario
def chaveMaiorValor(d):
    highestValueKey = None
    for key in d:
        if highestValueKey == None or d[key] > d[highestValueKey]:
            highestValueKey = key
    return highestValueKey

# qual o elemento mais comum numa lista
def mostCommonElement(l):
    d = contaElementos(l)
    return d[chaveMaiorValor(d)]

# uma lista é sublista de outra?

# dado 2 strings o fim de um é igual ao comeco do outro? (do projeto de prolog)
def verificaJoinStrings(s1, s2):
    if s1 == "" or s2 == "":
        return False
    elif s1[1:] == s2[:len(s2)-1]:
        return True
    return verificaJoinStrings(s1[1:], s2[:len(s2)-1])
