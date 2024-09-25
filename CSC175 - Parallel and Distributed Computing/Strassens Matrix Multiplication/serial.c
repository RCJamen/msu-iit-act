#include <stdio.h>
#include <stdlib.h>

// Global definitions to access the matrices
#define A(i, j) A[(i) * (n) + (j)]
#define B(i, j) B[(i) * (n) + (j)]
#define C(i, j) C[(i) * (n) + (j)]
#define ANSI_COLOR_RED "\x1b[31m"
#define ANSI_COLOR_RESET "\x1b[0m"

// Size of the matrix
const int n = 1048;

// Function definitions. Explained later.
double *Serial_Multiply(double *, double *, double *, int);
void print_matrix(double *, int, int);

int main() {
    double *A, *B, *C;
    int i, j;

    A = (double *)malloc(n * n * sizeof(double));
    B = (double *)malloc(n * n * sizeof(double));
    C = (double *)malloc(n * n * sizeof(double));

    // Set up the matrices
    for (i = 0; i < n; i++) {
        for (j = 0; j < n; j++) {
            A(i, j) = i + j;
            B(i, j) = i - j;
            C(i, j) = 0.0;
        }
    }

    C = Serial_Multiply(A, B, C, n);

    printf("/*********************************Serial multiplication***********************************************/ \n");
    print_matrix(C, n, n); //prints the product matrix in C

    free(A);
    free(B);
    free(C);

    return 0;
}

// Function to perform matrix multiplication
double *Serial_Multiply(double *A, double *B, double *C, int size) {
    int i, j, k;
    double sum;
    for (i = 0; i < size; i++) {
      for (j = 0; j < size; j++) {
        sum = 0;
        for (k = 0; k < size; k++) {
          sum += A[i * size + k] * B[k * size + j]; //C = A*B; 
        }
        C[i * size + j] = sum; //end inner most loop
      } // end middle loop
    } // end outer loop
    return C;
}


// Function to print the matrix
  static int count = 0; // used for colors
  void print_matrix(double * N, int rows, int columns) {

    int i, j;
    //print only 4*4 matrix.
    if (rows > 4)
      rows = 4;
    if (columns > 4)
      columns = 4;
    printf("Printing only first 4 results \n");
    for (i = 0; i < rows; i++) {
      for (j = 0; j < columns; j++) {
        if (count == 0)
          printf(ANSI_COLOR_RED "%lf \t", N[((n * i) + j)]); //row major accessing with red color.
      }
      printf(ANSI_COLOR_RESET "\n");
    }
    count++;
  }

