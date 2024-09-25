import numpy as np
import time

from mpi4py import MPI

n = 4096

start = time.time()

def brute_force(A, B):
    n, m, p = A.shape[0], A.shape[1], B.shape[1]
    C = np.array([[0]*p for i in range(n)], dtype=np.float64)
    for i in range(n):
        for j in range(p):
            for k in range(m):
                C[i][j] += A[i][k]*B[k][j]
    return C

def form_matrix(A, B, C, local_A, local_B, my_rank):
    if my_rank == 0:
        A = np.fromfunction(lambda i, j: i + j, (n, n), dtype=np.float64)
        B = np.fromfunction(lambda i, j: i - j, (n, n), dtype=np.float64)
        C = np.zeros((n, n), dtype=np.float64)
    else:
        A = np.empty((n,n), dtype=np.float64)
        B = np.empty((n,n), dtype=np.float64)
    comm = MPI.COMM_WORLD
    comm.Bcast(A, root=0)
    comm.Bcast(B, root=0)
    
    return A, B, C

def split(A, B, m, n):
    A11 = A[:m, :m]
    A12 = A[:m, m:]
    A21 = A[m:, :m]
    A22 = A[m:, m:]

    B11 = B[:m, :m]
    B12 = B[:m, m:]
    B21 = B[m:, :m]
    B22 = B[m:, m:]

    return A11, A12, A21, A22, B11, B12, B21, B22

def strassen(A, B, m, my_rank):
    n = A.shape[0]
    m = n // 2

    if len(A) <= 2:
        return brute_force(A, B)

    A11, A12, A21, A22, B11, B12, B21, B22 = split(A, B, m, n)

    p1 = np.empty((m,m), dtype=np.float64)
    p2 = np.empty((m,m), dtype=np.float64)
    p3 = np.empty((m,m), dtype=np.float64)
    p4 = np.empty((m,m), dtype=np.float64)
    p5 = np.empty((m,m), dtype=np.float64)
    p6 = np.empty((m,m), dtype=np.float64)
    p7 = np.empty((m,m), dtype=np.float64)

    comm = MPI.COMM_WORLD
    my_rank = comm.Get_rank()
   
    if my_rank == 0:
        p1 = brute_force(np.add(A11,A22), np.add(B11,B22))
        p7 = brute_force(np.subtract(A12,A22), np.add(B21,B22))

        comm.Recv(p2, source=2, tag=2)
        comm.Recv(p3, source=1, tag=3)
        comm.Recv(p4, source=2, tag=4)
        comm.Recv(p5, source=1, tag=5)
        comm.Recv(p6, source=3, tag=6)

        C11 = np.add(np.subtract(np.add(p1,p4), p5),p7)
        C12 = np.add(p3,p5)
        C21 = np.add(p2,p4)
        C22 = np.subtract(np.subtract(np.add(p3, p1), p2), p6)

        C = np.vstack((np.hstack((C11, C12)), np.hstack((C21, C22))))

        #print("Matrix A:")
        #print(A)
        #print("Matrix B:")
        #print(B)
    
        #print("Resultant Matrix:")
        #print(C)
        
        end = time.time()
        
        print("The time of execution of above program is :",
      (end-start), "s")
        
    elif my_rank == 1:
        p3 = brute_force(A11, np.subtract(B12,B22))
        p5 = brute_force(np.add(A11,A12), B22)

        comm.Send(p3, dest=0, tag=3)
        comm.Send(p5, dest=0, tag=5)

    elif my_rank == 2:
        p2 = brute_force(np.add(A21,A22), B11)
        p4 = brute_force(A22, np.subtract(B21,B11))

        comm.Send(p2, dest=0, tag=2)
        comm.Send(p4, dest=0, tag=4)
    
    elif my_rank == 3:
        p6 = brute_force(np.subtract(A11,A21), np.add(B11,B12))

        comm.Send(p6, dest=0, tag=6)

def main():
    comm = MPI.COMM_WORLD
    my_rank = comm.Get_rank()
    p = comm.Get_size()

    start_time = MPI.Wtime()

    A = None
    B = None
    C = None
    local_A = np.empty((n // p, n), dtype=np.float64)
    local_B = np.empty((n, n), dtype=np.float64)
    local_C = np.empty((n // p, n), dtype=np.float64)

    A, B, C = form_matrix(A, B, C, local_A, local_B, my_rank)

    comm.Barrier()
    
    strassen(A, B, n, my_rank)

if __name__ == '__main__':
    main()
