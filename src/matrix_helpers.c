#include <stdio.h>
#include <R.h>

/*
 * Debug use only. Print matrix to stdout
 */
void print_matrix(double *X, size_t m, size_t n)
{
        for(int i = 0; i < m; i++) {
                Rprintf("%d:", i);
                for(int j = 0; j < n; j++) {
                        Rprintf("\t%.4f", X[m*j + i]);
                }
                Rprintf("\n");
        }
        Rprintf("\n");
}

