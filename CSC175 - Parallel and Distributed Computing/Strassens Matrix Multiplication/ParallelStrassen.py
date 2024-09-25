import numpy as np
from mpi4py import MPI

# Initialize the MPI environment
comm = MPI.COMM_WORLD
my_rank = comm.Get_rank()  # returns the rank of individual processors
p = comm.Get_size()  # returns the number of processors

n = 1024    #number of size


def A(i, j):
    return A[i * n + j]
def B(i, j):
    return B[i * n + j]
def C(i, j):
    return C[i * n + j]

if my_rank == 0:  # master process
    for i in range(n):
        for j in range(n):
            A[i, j] = i + j  # As used in Cannon's algorithm for testing purposes
            B[i, j] = i - j  # As used in Cannon's algorithm
            C[i, j] = 0.0  # Initialize C with 0's

comm.Barrier()

#Start for Strassen
rows = n // 2
columns = n // 2
no_of_elements = rows * columns
starts = [0, 0]  # Starting position of the subarray
subns = [rows, columns]  # Size of the subarray
bigns = [n, n]  # Total size of the big array

mysubarray = MPI.DOUBLE.Create_subarray(bigns, subns, starts, order=MPI.ORDER_C)
mysubarray.Commit()

subarrtype = mysubarray.Create_resized(0, no_of_elements * MPI.DOUBLE.size)
subarrtype.Commit()

# Allocate local matrices
local_A = np.zeros(no_of_elements, dtype=np.float64)
local_B = np.zeros(no_of_elements, dtype=np.float64)
local_C = np.zeros(no_of_elements, dtype=np.float64)






def parallel_multiply(A, B, C, root, my_rank, no_of_processors, rows, columns):
    from_val = my_rank * rows // no_of_processors
    to_val = (my_rank + 1) * rows // no_of_processors

    if rows % no_of_processors != 0:
        if my_rank == root:
            print("Matrix size not divisible by number of processors")
        exit(-1)

    no_of_elements = rows * columns
    result = np.zeros(no_of_elements, dtype=np.float64)

    comm = MPI.COMM_WORLD
    B = comm.bcast(B, root=root)

    A_local = np.zeros(no_of_elements // no_of_processors, dtype=np.float64)
    comm.Scatter(A, A_local, root=root)

    for i in range(from_val, to_val):
        for j in range(columns):
            sum_val = 0.0
            for k in range(rows):
                sum_val += A_local[(i - from_val) * rows + k] * B[k * rows + j]
            result[i * rows + j] = sum_val

    comm.Gather(result, C, root=root)

    return C


