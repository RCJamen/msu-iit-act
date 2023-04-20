"""
Ramel Cary B. Jamen
2019-2093
April 18, 2023
Write a routine implementing parallel matrix-vector product
y= matvec(comm,A,x).
Tip: use Comm.Allgather() and numpy.dot()
"""

from mpi4py import MPI
import numpy as np

comm = MPI.COMM_WORLD
size = comm.Get_size()

def matvec(comm, A, x):
    m = A.shape[0] 
    p = size
    xg = np.zeros(m*p, dtype='d')
    comm.Allgather([x,  MPI.DOUBLE],
                   [xg, MPI.DOUBLE])
    y = np.dot(A, xg)
    return y
