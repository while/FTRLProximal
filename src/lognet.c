#include <R.h>
#include <R_ext/BLAS.h>

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "list.h"
#include "matrix_helpers.h"

#define DEBUG 0

#define NUM_THREADS 10000
#define BLOCK_WIDTH 100

#define SQR(x)  ((x)*(x))
#define ABS(x)  (((x) < 0) ? -(x) : (x))
#define SGN(x)  (((x) < 0) ? -1. : 1.)

/*
 * Sigmoid function
 */
double sigmoid(double x)
{
        return 1./(1. + exp(-x));
}


/*
 * Predict outcome given a set of feature vectors and the model coefficients
 */
void lognet_predict(double *X, double *theta, double *yhat, int *m, int *n)
{
        // Define constants needed for BLAS routine below
        double a = 1.0;
        double b = 0.0;
        int inc = 1;

        // Call BLAS riutine for matrix vector multiplication DGEMV
        // computing X*theta saving the result in yhat
        F77_CALL(dgemv)("n", m, n, &a,
                    X, m, theta, &inc, &b, yhat, &inc);

        // Transform result from logodds to prob
        for (int i=0; i < (*m); i++) {
                yhat[i] = sigmoid(yhat[i]);
        }
}


/*
 * Online elastic net logistic regression trained using FTRL-Proximal
 * see https://www.eecs.tufts.edu/~dsculley/papers/ad-click-prediction.pdf
 */
void lognet_ftrlprox(double *X, double *theta, double *y, unsigned int *m, 
                     unsigned int *n, double *J, unsigned int *num_epochs,
                     double *alpha, double *bnn, double *lambda1,
                     double *lambda2, unsigned int *loss)

{
        if (DEBUG)  {
                printf("num_epochs: %u\n", *num_epochs);
                printf("num_itr: %u\n", ((*m)*(*num_epochs)));
                print_matrix(X, (*m), (*n));
        }

        // Initialize z and nn
        double z[*n];
        double nn[*n];
        for (int i = 0; i < (*n); i++) {
          z[i]  = 0.;
          nn[i] = 0.;
        }

        for (int t = 0; t < ((*m)*(*num_epochs)); t++) {
                double x[*n];
                list_t *idx = list();

                // Extract feature vector and indexes where xi is not 0
                // as those will not help update the coefficients
                for (int i = 0; i < (*n); i++) {
                        x[i] = X[(*m)*i + (t%(*m))];
                        if (x[i] != 0.) append(idx, i);
                }

                // Loop over non zero indices
                node_t *li = idx->first;
                while(li != NULL) {
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
                //J[t] = -y[t%(*m)]*log(yhat) - (1. - y[t%(*m)])*log(1. - yhat);

                // Loop over list again to update gradient and learning rate 
                double g[*n];
                double sig[*n];
                li = idx->first;
                while(li != NULL) {
                        int i = li->val;
                        li = li->next;

                        g[i] = (yhat - y[t%(*m)])*x[i];
                        sig[i] = (sqrt(nn[i] + SQR(g[i])) - sqrt(nn[i]))/(*alpha);
                        z[i] += g[i] - sig[i]*theta[i];
                        nn[i] += SQR(g[i]);
                }

                erase(idx);
        }
}

int main() 
{
        double X[] = {
                1, -0.474208, -2.295092,
                1,  0.247492, -1.194799,
                1,  0.204377, -1.191244,
                1,  0.803192,  1.562826,
                1, -0.678238, -1.372498,
                1, -2.878567, -1.902228,
                1, -2.100316, -0.543889,
                1, -0.201629, -0.756559,
                1, -1.525376, -2.040805,
                1, -2.752720,  0.112114
        };

        double theta[] = {0., 0., 0.};
        double y[] = {
                1.,
                1.,
                1.,
                0.,
                1.,
                1.,
                1.,
                1.,
                1.,
                1.
        };
        unsigned int m = 10;
        unsigned int n = 3;
        double J[] = {0.};
        unsigned int num_epochs = 1000;
        double alpha = 1.;
        double beta = 1.;
        double lambda1 = 0.;
        double lambda2 = 0.;
        int loss = 0;

        lognet_ftrlprox(X, theta, y, &m, &n, J, &num_epochs,
                        &alpha, &beta, &lambda1, &lambda2, &loss);
        print_matrix(theta, 1, n);
}

