# https://en.wikipedia.org/wiki/Floyd%E2%80%93Warshall_algorithm
# https://www.youtube.com/watch?v=4OQeCuLYj-4

def leGrafo():
    graph = {}
    while True:
        try:
            entry = input()
        except EOFError as e:
            break
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
    print(grafo)
    #viagens = leViagens()

if __name__ == "__main__":
    main()
