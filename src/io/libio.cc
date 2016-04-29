#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "libio.h"

void print_dvector(double* V, int m) {
    int i;
    for (i=0; i<m; i++) {
         fprintf(stdout, "%14.6f", V[i]);
    }
    fprintf(stdout, "\n");
};

void print_dmatrix(double* A, int m, int n) {
    int i, j, ij;
    for (i=0; i<m; i++) {
       for (j=0; j<n; j++) {
           ij = i*n + j ;
           fprintf(stdout, "%14.6f", A[ij]);
       }
       fprintf(stdout, "\n");
    }
};
