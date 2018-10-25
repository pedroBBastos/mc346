# https://docs.scipy.org/doc/scipy/reference/generated/scipy.sparse.csgraph.floyd_warshall.html#scipy.sparse.csgraph.floyd_warshall

def leGrafo():
    graph = {}
    while True:
        entry = input()
        if entry == "":
            break
        e = entry.split()
        if e[0] not in graph:
            graph[e[0]] = [(e[1], e[2])]
        else:
            graph[e[0]].append((e[1], e[2]))
    return graph

def main():
    grafo = leGrafo()
    viagens = leViagens()

if __name__ == "__main__":
    main()
