import numpy as np
import time

# Start measuring execution time
st = time.process_time()

# Define a function for the brute force matrix multiplication
def brute_force(A, B):
    n, m, p = A.shape[0], A.shape[1], B.shape[1]
    C = np.array([[0]*p for i in range(n)])
    for i in range(n):
        for j in range(p):
            for k in range(m):
                C[i][j] += A[i][k]*B[k][j]
    return C

# Define a function to split a matrix into four quadrants
def split(matrix):
    n = len(matrix)
    return matrix[:n//2, :n//2], matrix[:n//2, n//2:], matrix[n//2:, :n//2], matrix[n//2:, n//2:]

# Define the Strassen matrix multiplication algorithm
def strassen(A, B, m):
    if len(A) <= 2:
        return brute_force(A, B)
    
    A11, A12, A21, A22 = split(A)
    B11, B12, B21, B22 = split(B)

    p1 = strassen(np.add(A11,A22), np.add(B11,B22), m)
    p2 = strassen(np.add(A21,A22), B11, m)
    p3 = strassen(A11, np.subtract(B12,B22), m)
    p4 = strassen(A22, np.subtract(B21,B11), m)
    p5 = strassen(np.add(A11,A12), B22, m)
    p6 = strassen(np.subtract(A11,A21), np.add(B11,B12), m)
    p7 = strassen(np.subtract(A12,A22), np.add(B21,B22), m)

    C11 = np.add(np.subtract(np.add(p1,p4), p5),p7)
    C12 = np.add(p3,p5)
    C21 = np.add(p2,p4)
    C22 = np.subtract(np.subtract(np.add(p3, p1), p2), p6)

    C = np.vstack((np.hstack((C11, C12)), np.hstack((C21, C22))))
    C = C[:m, :m]
    return C

# Test the algorithm
nsize = 1024
A = []
B = []

for i in range(nsize):
    Arow = np.random.randint(100, size=nsize).tolist()
    A.append(Arow)
    Brow = np.random.randint(100, size=nsize).tolist()
    B.append(Brow)
A = np.asarray(A)
B = np.asarray(B)

n = len(A)

# result = strassen(A, B, n)
result = brute_force(A,B)
print("Matrix A:")
print(A)
print("Matrix B:")
print(B)

print("Resultant Matrix:")
print(result)

et = time.process_time()
elapsed_time = et - st
print('Execution time:', elapsed_time, 'seconds')