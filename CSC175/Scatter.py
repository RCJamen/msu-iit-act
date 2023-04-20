"""
Ramel Cary B. Jamen
2019-2093
April 18, 2023
Modified Scatter to communicate Numpy Arrays
"""

from mpi4py import MPI
import numpy as np

comm = MPI.COMM_WORLD
size = comm.Get_size()
rank = comm.Get_rank()

if rank == 0:
    sendmsg = np.arange(size, dtype='i')
else:
    sendmsg = np.zeros(size, dtype='i')

print ("Intitial messsage:", sendmsg, "on process", rank)

recvmsg = comm.scatter(sendmsg, root=0)         
assert np.allclose(recvmsg, rank)

print ("I have recieved", recvmsg, "on process", rank)
