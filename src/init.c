#include <R.h>
#include <Rinternals.h>
#include <stdlib.h>
#include <R_ext/Rdynload.h>

/* Forward declarations of the C entry points called via .C() */
extern void lognet_predict(double *X, double *theta, double *yhat,
                           int *m, int *n);

extern void lognet_ftrlprox(double *X, double *theta, double *y,
                            int *m, int *n, double *z, double *nn,
                            double *J, unsigned int *num_epochs,
                            double *alpha, double *bnn,
                            double *lambda1, double *lambda2,
                            unsigned int *save_loss);

extern void splognet_ftrlprox(double *X, int *ix, int *jx,
                              double *theta, double *y,
                              int *m, int *n, double *z, double *nn,
                              double *J, unsigned int *num_epochs,
                              double *alpha, double *bnn,
                              double *lambda1, double *lambda2,
                              unsigned int *save_loss);

static const R_CMethodDef CEntries[] = {
    {"lognet_predict",     (DL_FUNC) &lognet_predict,      5},
    {"lognet_ftrlprox",    (DL_FUNC) &lognet_ftrlprox,    14},
    {"splognet_ftrlprox",  (DL_FUNC) &splognet_ftrlprox,  16},
    {NULL, NULL, 0}
};

void R_init_FTRLProximal(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
