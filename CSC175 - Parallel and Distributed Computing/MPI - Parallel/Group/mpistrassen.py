from mpi4py import MPI
import numpy as np
import time

# Get the MPI rank and size
comm = MPI.COMM_WORLD
rank = comm.Get_rank()
size = comm.Get_size()
RankMaster = 0 

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
    
    if rank == RankMaster:
        A11, A12, A21, A22 = split(A)
        B11, B12, B21, B22 = split(B)
        comm.send(A11, dest=1, tag=1)
        comm.send(A11, dest=3, tag=3)
        comm.send(A11, dest=5, tag=5)
        comm.send(A11, dest=6, tag=6)
        comm.send(A12, dest=5, tag=5)
        comm.send(A12, dest=7, tag=7)
        comm.send(A21, dest=2, tag=2)
        comm.send(A21, dest=6, tag=6)
        comm.send(A22, dest=1, tag=1)
        comm.send(A22, dest=2, tag=2)
        comm.send(A22, dest=4, tag=4)
        comm.send(A22, dest=7, tag=7)
        comm.send(B11, dest=1, tag=1)
        comm.send(B11, dest=2, tag=2)
        comm.send(B11, dest=4, tag=4)
        comm.send(B11, dest=6, tag=6)
        comm.send(B12, dest=3, tag=3)
        comm.send(B12, dest=6, tag=6)
        comm.send(B21, dest=4, tag=4)
        comm.send(B21, dest=7, tag=7)
        comm.send(B22, dest=1, tag=1)
        comm.send(B22, dest=3, tag=3)
        comm.send(B22, dest=5, tag=5)
        comm.send(B22, dest=7, tag=7)
    
        comm.recv(source=1, tag=0)
        comm.recv(source=2, tag=0)
        comm.recv(source=3, tag=0)
        comm.recv(source=4, tag=0)
        comm.recv(source=5, tag=0)
        comm.recv(source=6, tag=0)
        comm.recv(source=7, tag=0)
        C11 = np.add(np.subtract(np.add(p1,p4), p5),p7)
        C12 = np.add(p3,p5)
        C21 = np.add(p2,p4)
        C22 = np.subtract(np.subtract(np.add(p3, p1), p2), p6)

        C = np.vstack((np.hstack((C11, C12)), np.hstack((C21, C22))))
        C = C[:m, :m]
        return C    
    
    # Workers   
    elif rank == 1:
        comm.recv(source=RankMaster, tag=1)
        p1 = strassen(np.add(A11,A22), np.add(B11,B22), m)
        comm.send(p1, dest=RankMaster, tag=0)
    elif rank == 2:
        comm.recv(source=RankMaster, tag=2)
        p2 = strassen(np.add(A21,A22), B11, m)
        comm.send(p2, dest=RankMaster, tag=0)
    elif rank == 3:
        comm.recv(source=RankMaster, tag=3)
        p3 = strassen(A11, np.subtract(B12,B22), m)
        comm.send(p3, dest=RankMaster, tag=0)
    elif rank == 4:
        comm.recv(source=RankMaster, tag=4)
        p4 = strassen(A22, np.subtract(B21,B11), m)
        comm.send(p4, dest=RankMaster, tag=0)
    elif rank == 5:
        comm.recv(source=RankMaster, tag=5)
        p5 = strassen(np.add(A11,A12), B22, m)
        comm.send(p5, dest=RankMaster, tag=0)
    elif rank == 6:
        comm.recv(source=RankMaster, tag=6)
        p6 = strassen(np.subtract(A11,A21), np.add(B11,B12), m)
        comm.send(p6, dest=RankMaster, tag=0)
    elif rank == 7:
        comm.recv(source=RankMaster, tag=7)
        p7 = strassen(np.subtract(A12,A22), np.add(B21,B22), m)
        comm.send(p7, dest=RankMaster, tag=0)


nsize = 64
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

result = strassen(A, B, n)

# if RankMaster == 0:
print("Matrix A:")
print(A)
print("Matrix B:")
print(B)
print("Resultant Matrix:")
print(result)

et = time.process_time()
elapsed_time = et - st

print('Execution time:', elapsed_time, 'seconds')