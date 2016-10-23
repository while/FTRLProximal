#include <R.h>
#include <R_ext/BLAS.h>

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "list.h"
#include "matrix_helpers.h"

#define DEBUG 0

#define SQR(x)  ((x)*(x))
#define ABS(x)  (((x) < 0) ? -(x) : (x))
#define SGN(x)  (((x) < 0) ? -1. : 1.)

/* Declare lognet's predct function as it is needed here as well. */
void lognet_predict(double *X, double *theta, double *yhat, int *m, int *n);

/*
 * Predict outcome given a set of feature vectors and the model coefficients
 */
void splognet_predict(double *X, double *theta, double *yhat, int *m, int *n)
{
        // TODO: implement splognet predict
}


/*
 * Online elastic net logistic regression trained using FTRL-Proximal
 * see https://www.eecs.tufts.edu/~dsculley/papers/ad-click-prediction.pdf
 */
void splognet_ftrlprox(double *X, int *ix, int* jx, double *theta, double *y,
                       unsigned int *m, unsigned int *n, double *z, double *nn, 
                       double *J, unsigned int *num_epochs, double *alpha,
                       double *bnn, double *lambda1, double *lambda2,
                       unsigned int *save_loss)

{
        if (DEBUG)  {
                printf("num_epochs: %u\n", *num_epochs);
                printf("num_itr: %u\n", ((*m)*(*num_epochs)));
                print_matrix(X, (*m), (*n));
        }

        // Initialize z and nn
        double *g = malloc((*n)*sizeof(double));
        double *sig = malloc((*n)*sizeof(double));
        double *x = malloc((*n)*sizeof(double));

        for (int t = 0; t < ((*m)*(*num_epochs)); t++) {
                node_t *l1 = malloc(sizeof(node_t));
                l1->next = NULL;
                node_t *li = l1;


                // Extract feature vector and indexes where xi is not 0
                // as those will not help update the coefficients
                for (int i = 0; i < (*n); i++) {
                        // a bit of index fiddeling is needed here to handle
                        // sparse matrices
                        if (jx[ix[i]] == t) {
                                x[i] = X[ix[i]];
                                li->val = i;
                                li->next = malloc(sizeof(node_t));
                                li = li->next;
                                li->next = NULL;
                                ix[i]++;
                        } else {
                                x[i] = 0.;
                        }
                }

                if (DEBUG) {
                        printf("x = [");
                        for (int i = 0; i < (*n); i++) {
                                printf(" %.3f ", x[i]);
                        }
                        printf("]\n");
                }

                // Loop over non zero indices
                li = l1;
                while(li->next != NULL) {
                        // move list pointer
                        int i = li->val;
                        li = li->next;

                        // Update coefficients
                        theta[i] = (ABS(z[i]) <= (*lambda1)) ? 0. : -(z[i] - SGN(z[i])*(*lambda1))/(((*bnn) + sqrt(nn[i]))/(*alpha) + (*lambda2));

                }

                // Predict using new theta
                double yhat;
                int m1 = 1;
                lognet_predict(x, theta, &yhat, &m1, n);

                // Save cost function
                if (*save_loss) {
                        J[t] = -y[t%(*m)]*log(yhat) - (1. - y[t%(*m)])*log(1. - yhat);
                }

                // Loop over list again to update gradient and learning rate 
                li = l1;
                while(li->next != NULL) {
                        int i = li->val;
                        li = li->next;

                        g[i] = (yhat - y[t%(*m)])*x[i];
                        sig[i] = (sqrt(nn[i] + SQR(g[i])) - sqrt(nn[i]))/(*alpha);
                        z[i] += g[i] - sig[i]*theta[i];
                        nn[i] += SQR(g[i]);
                }

                // Free index list
                li = l1;
                while(li != NULL) {
                        l1 = li->next;
                        free(li);
                        li = l1;
                }
        }

        // Free up used mem
        free(g);
        free(sig);
        free(x);
}

