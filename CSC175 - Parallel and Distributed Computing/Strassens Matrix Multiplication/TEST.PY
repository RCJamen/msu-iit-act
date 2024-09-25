from mpi4py import MPI
import numpy as np
import random

def print_matrix(mat):
    n = len(mat)
    for i in range(n):
        for j in range(n):
            print(mat[i][j], end=" ")
        print()
    print()

def fill_matrix(n, mat):
    for i in range(n):
        for j in range(n):
            mat[i][j] = random.randint(1, 50)

def get_slice(n, mat, offset_i, offset_j):
    slice_mat = [[0 for _ in range(n // 2)] for _ in range(n // 2)]
    for i in range(n // 2):
        for j in range(n // 2):
            slice_mat[i][j] = mat[offset_i + i][offset_j + j]
    return slice_mat

def add_matrices(n, mat1, mat2, add):
    result = allocate_matrix(n)
    for i in range(n):
        for j in range(n):
            if add:
                result[i][j] = mat1[i][j] + mat2[i][j]
            else:
                result[i][j] = mat1[i][j] - mat2[i][j]
    return result

def allocate_matrix(n):
    data = [[0] * n for _ in range(n)]
    array = [row for row in data]
    return array

def combine_matrices(m, c11, c12, c21, c22):
    n = 2 * m
    result = allocate_matrix(n)

    for i in range(n):
        for j in range(n):
            if i < m and j < m:
                result[i][j] = c11[i][j]
            elif i < m:
                result[i][j] = c12[i][j - m]
            elif j < m:
                result[i][j] = c21[i - m][j]
            else:
                result[i][j] = c22[i - m][j - m]

    return result

def naive(n, mat1, mat2):
    prod = [[0 for _ in range(n)] for _ in range(n)]

    for i in range(n):
        for j in range(n):
            prod[i][j] = 0
            for k in range(n):
                prod[i][j] += mat1[i][k] * mat2[k][j]

    return prod

def check(n, prod1, prod2):
    for i in range(n):
        for j in range(n):
            if prod1[i][j] != prod2[i][j]:
                return False
    return True

def strassen2(n, mat1, mat2):
    if n <= 32:
        return naive(n, mat1, mat2)

    m = n // 2

    a = get_slice(n, mat1, 0, 0)
    b = get_slice(n, mat1, 0, m)
    c = get_slice(n, mat1, m, 0)
    d = get_slice(n, mat1, m, m)
    e = get_slice(n, mat2, 0, 0)
    f = get_slice(n, mat2, 0, m)
    g = get_slice(n, mat2, m, 0)
    h = get_slice(n, mat2, m, m)

    bds = add_matrices(m, b, d, False)
    gha = add_matrices(m, g, h, True)
    s1 = strassen(m, bds, gha)

    ada = add_matrices(m, a, d, True)
    eha = add_matrices(m, e, h, True)
    s2 = strassen(m, ada, eha)

    acs = add_matrices(m, a, c, False)
    efa = add_matrices(m, e, f, True)
    s3 = strassen(m, acs, efa)

    aba = add_matrices(m, a, b, True)
    s4 = strassen(m, aba, h)

    fhs = add_matrices(m, f, h, False)
    s5 = strassen(m, a, fhs)

    ges = add_matrices(m, g, e, False)
    s6 = strassen(m, d, ges)

    cda = add_matrices(m, c, d, True)
    s7 = strassen(m, cda, e)

    s1s2a = add_matrices(m, s1, s2, True)
    s6s4s = add_matrices(m, s6, s4, False)
    c11 = add_matrices(m, s1s2a, s6s4s, True)

    c12 = add_matrices(m, s4, s5, True)

    c21 = add_matrices(m, s6, s7, True)

    s2s3s = add_matrices(m, s2, s3, False)
    s5s7s = add_matrices(m, s5, s7, False)
    c22 = add_matrices(m, s2s3s, s5s7s, True)

    prod = combine_matrices(m, c11, c12, c21, c22)

    return prod



def strassen(n, mat1, mat2, prod, p_rank):
    if n == 1:
        prod[0][0] = mat1[0][0] * mat2[0][0]
        return

    m = n // 2

    a = get_slice(n, mat1, 0, 0)
    b = get_slice(n, mat1, 0, m)
    c = get_slice(n, mat1, m, 0)
    d = get_slice(n, mat1, m, m)
    e = get_slice(n, mat2, 0, 0)
    f = get_slice(n, mat2, 0, m)
    g = get_slice(n, mat2, m, 0)
    h = get_slice(n, mat2, m, m)

    s1 = allocate_matrix(m)
    s2 = allocate_matrix(m)
    s3 = allocate_matrix(m)
    s4 = allocate_matrix(m)
    s5 = allocate_matrix(m)
    s6 = allocate_matrix(m)
    s7 = allocate_matrix(m)

    if p_rank == 0:
        s1 = np.empty((m, m), dtype=np.int32)
        s2 = np.empty((m, m), dtype=np.int32)
        s3 = np.empty((m, m), dtype=np.int32)
        s4 = np.empty((m, m), dtype=np.int32)
        s5 = np.empty((m, m), dtype=np.int32)
        s6 = np.empty((m, m), dtype=np.int32)
        s7 = np.empty((m, m), dtype=np.int32)

        MPI.COMM_WORLD.Recv([s1, MPI.INT], source=1, tag=0)
        MPI.COMM_WORLD.Recv([s2, MPI.INT], source=2, tag=0)
        MPI.COMM_WORLD.Recv([s3, MPI.INT], source=3, tag=0)
        MPI.COMM_WORLD.Recv([s4, MPI.INT], source=4, tag=0)
        MPI.COMM_WORLD.Recv([s5, MPI.INT], source=5, tag=0)
        MPI.COMM_WORLD.Recv([s6, MPI.INT], source=6, tag=0)
        MPI.COMM_WORLD.Recv([s7, MPI.INT], source=7, tag=0)

    if p_rank == 1:
        bds = add_matrices(m, b, d, False)
        gha = add_matrices(m, g, h, True)
        s1 = strassen2(m, bds, gha)
        MPI.COMM_WORLD.Send([s1, MPI.INT], dest=0, tag=0)

    if p_rank == 2:
        ada = add_matrices(m, a, d, True)
        eha = add_matrices(m, e, h, True)
        s2 = strassen2(m, ada, eha)
        MPI.COMM_WORLD.Send([s2, MPI.INT], dest=0, tag=0)

    if p_rank == 3:
        acs = add_matrices(m, a, c, False)
        efa = add_matrices(m, e, f, True)
        s3 = strassen2(m, acs, efa)
        MPI.COMM_WORLD.Send([s3, MPI.INT], dest=0, tag=0)

    if p_rank == 4:
        aba = add_matrices(m, a, b, True)
        s4 = strassen2(m, aba, h)
        MPI.COMM_WORLD.Send([s4, MPI.INT], dest=0, tag=0)


    if p_rank == 5:
        fhs = add_matrices(m, f, h, False)
        s5 = strassen2(m, a, fhs)
        MPI.COMM_WORLD.Send([s5, MPI.INT], dest=0, tag=0)

    if p_rank == 6:
        ges = add_matrices(m, g, e, False)
        s6 = strassen2(m, d, ges)
        MPI.COMM_WORLD.Send([s6, MPI.INT], dest=0, tag=0)

    if p_rank == 7:
        cda = add_matrices(m, c, d, True)
        s7 = strassen2(m, cda, e)
        MPI.COMM_WORLD.Send([s7, MPI.INT], dest=0, tag=0)

    MPI.COMM_WORLD.Barrier()

    if p_rank == 0:
        s1s2a = add_matrices(m, s1, s2, True)
        s6s4s = add_matrices(m, s6, s4, False)
        c11 = add_matrices(m, s1s2a, s6s4s, True)

        c12 = add_matrices(m, s4, s5, True)

        c21 = add_matrices(m, s6, s7, True)

        s2s3s = add_matrices(m, s2, s3, False)
        s5s7s = add_matrices(m, s5, s7, False)
        c22 = add_matrices(m, s2s3s, s5s7s, True)

        prod = combine_matrices(m, c11, c12, c21, c22)


if __name__ == "__main__":
    comm = MPI.COMM_WORLD
    p_rank = comm.Get_rank()
    num_process = comm.Get_size()

    n = 8

    if p_rank == 0:
        print()
        n = int(input("Enter the dimensions of the matrix: "))

    comm.Barrier()
    n = comm.bcast(n, root=0)

    if p_rank == 0:
        mat1 = allocate_matrix(n)
        mat2 = allocate_matrix(n)
        fill_matrix(n, mat1)
        fill_matrix(n, mat2)
    else:
        mat1 = None
        mat2 = None

    comm.Barrier()
    mat1 = comm.bcast(mat1, root=0)
    mat2 = comm.bcast(mat2, root=0)

    comm.Barrier()

    startTime = MPI.Wtime()

    prod = np.zeros((n, n), dtype=np.int32)
    strassen(n, mat1, mat2, prod, p_rank)

    endTime = MPI.Wtime()

    if p_rank == 0:
        print("\nParallel Strassen Runtime (MPI): ", endTime - startTime)
        print()

    MPI.Finalize()
    