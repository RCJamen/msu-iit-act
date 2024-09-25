import numpy as np

nsize = 4
A = []
B = []

for i in range(nsize):
    Arow = np.random.randint(100, size=nsize).tolist()
    A.append(Arow)
    Brow = np.random.randint(100, size=nsize).tolist()
    B.append(Brow)

print (A)
print (B)

print (len(A))
print (len(B))
