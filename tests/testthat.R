library(testthat)
library(FTRLProximal)

# Tests use set.seed(1) to produce deterministic training data via
# mlbench. R 3.6.0 changed the sample() algorithm, which alters the
# class labels assigned by mlbench and would otherwise invalidate the
# hard-coded coefficient expectations in these tests. Pin the RNG to the
# pre-3.6 version so the regression tests remain reproducible.
suppressWarnings(RNGversion("3.5.0"))

test_check("FTRLProximal")
