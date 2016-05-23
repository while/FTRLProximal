#include <stdio.h>

/*
 * Debug use only. Print matrix to stdout
 */
void print_matrix(double *X, size_t m, size_t n)
{
        for(int i = 0; i < m; i++) {
                printf("%d:", i);
                for(int j = 0; j < n; j++) {
                        printf("\t%.4f", X[m*j + i]);
                }
                putchar('\n');
        }
        putchar('\n');
}

