"""
Ramel Cary B. Jamen
2019-2093
April 12, 2023
Modified Exchange to communicate Numpy Arrays
Tip: use comm.Isend() and comm.Irecv()
"""

from mpi4py import MPI
import numpy as np

comm = MPI.COMM_WORLD

send_arr = np.array([comm.rank] * comm.size)
# print (send_arr)
right = (comm.rank + 1) % comm.size     # 1,2,3,0
left = (comm.rank - 1) % comm.size      # 3,0,1,2

req1 = comm.Isend(send_arr, dest=right)
req2 = comm.Isend(send_arr, dest=left)

recv_arr_left = np.empty(comm.size, dtype=np.int64)
recv_arr_right = np.empty(comm.size, dtype=np.int64)

lmsg = comm.Irecv(recv_arr_left, source=left)
rmsg = comm.Irecv(recv_arr_right, source=right)

MPI.Request.Waitall([req1, req2, lmsg, rmsg])

assert np.array_equal(recv_arr_left, np.array([left] * comm.size))
assert np.array_equal(recv_arr_right, np.array([right] * comm.size))


