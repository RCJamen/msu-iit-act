"""
Ramel Cary B. Jamen
2019-2093
April 18, 2023
Modified Broadcast to communicate Numpy Arrays
"""

from mpi4py import MPI
import numpy as np

comm = MPI.COMM_WORLD
rank = comm.Get_rank()


if rank == 0:
    sendmsg = np.arange(comm.size, dtype='i')    #comm.size as number of array range
else:
    sendmsg = np.empty(comm.size, dtype='i')                                     

print ("Intitial messsage:", sendmsg, "on process", rank)

comm.Bcast(sendmsg, root=0)

for i in range(comm.size):
    assert sendmsg[i] == i

print ("I have recieved", sendmsg, "on process", rank)
