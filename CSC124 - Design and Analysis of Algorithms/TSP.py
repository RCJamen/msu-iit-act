import sys
import numpy as np

def dfs(solns, graph, path, u):
    if len(graph) == len(path):
        solns.append(path[:])
    else:
        for v in graph[u].keys():
            if v not in path:
                path.append(v)
                dfs(solns, graph, path, v)
                path.remove(v)

def sp(solns, graph):
    w = np.full(len(solns), sys.maxsize)
    for s, soln in enumerate(solns):
        w[s] = sum(graph[soln[i]][soln[i+1]] for i in range(len(soln)-1))
    return w.min(), np.argmin(w)

graph = {
    '1': {'2': 10, '4': 13, '5': 5, '6': 11, '8': 8},
    '2': {'1': 10, '3': 6, '4': 5, '5': 6, '7': 1},
    '3': {'2': 6, '4': 10, '6': 15, '7': 8, '10': 9},
    '4': {'1': 13, '2': 5, '3': 10, '5': 13, '7': 13, '9': 8, '10': 12},
    '5': {'1': 5, '2': 6, '4': 13, '6': 4, '9': 3},
    '6': {'1': 11, '3': 15, '5': 4, '8': 6, '9': 9},
    '7': {'2': 1, '3': 8, '4': 13, '10': 5},
    '8': {'1': 8, '6': 6, '9': 7},
    '9': {'4': 8, '5': 3, '6': 9, '8': 7, '10': 9,},
    '10': {'3': 9, '4': 12, '7': 5, '9': 9,}
}

solns = []
dfs(solns, graph, ['1'], '1')
print(solns)
numOfSolns = len(solns)
w, indxs = sp(solns, graph)
print("Number of Possible Solutions:", numOfSolns)
print("Shortest path weight:", w)
print("Shortest path index:", indxs)
print("Shortest path:", solns[indxs])