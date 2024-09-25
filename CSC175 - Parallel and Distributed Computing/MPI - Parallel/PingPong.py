"""
Ramel Cary B. Jamen
2019-2093
April 12, 2023
Modified Pingpong to communicate Numpy Arrays
Tip: use comm.Send() and comm.Recv()
"""

from mpi4py import MPI
import numpy as np

comm = MPI.COMM_WORLD
assert comm.size == 2

if comm.rank == 0:
    array1 = np.arange(1000, dtype='f')
    array2 = np.empty(1000, dtype='f')
    # print (array1 , 'Source 1 = Main Array 1')
    print (array2 , 'Source 1 = Main Array 2 \n')
    comm.Send([array1, MPI.INT], dest=1, tag=55)
    comm.Recv([array2, MPI.INT], source=1, tag=77)
    print (array2, 'Source 1 = From Source 2 Array 1 \n')
else:
    array1 = np.arange(1000, dtype='f')
    array2 = np.empty(1000, dtype='f')
    # print (array1 , 'Source 2 = Main Array 1')
    print (array2, 'Source 2 = Main Array 2 \n')
    comm.Recv([array2, MPI.INT], source=0, tag=55)
    comm.Send([array1, MPI.INT], dest=0, tag=77)
    print (array2 , 'Source 2 = From Source 1 Array 1')

    