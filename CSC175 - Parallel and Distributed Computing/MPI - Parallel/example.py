from mpi4py import MPI
import numpy as np
import time

# record start time
start = time.time()

if __name__ == "__main__":
    N = 10000000

    # initialize a
    a = np.ones(N)

    # initialize b
    b = np.zeros(N)

    for i in range(N):
        b[i] = 1.0 + i

    # add two arrays

    for i in range(N):
        a[i] = a[i] + b[i]

    # average the result
    sum = 0.0
    for i in range(N):
        sum += a[i]
    average = sum / N

    print("Average: " + str(average))

end = time.time()

print("The time of execution of above program is :",
      (end-start) * 10**3, "ms")
