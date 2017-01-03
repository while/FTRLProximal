# FTRL Proximal
[![Build Status](https://travis-ci.org/while/FTRLProximal.svg?branch=master)](https://travis-ci.org/while/FTRLProximal)
[![codecov](https://codecov.io/github/while/FTRLProximal/branch/master/graphs/badge.svg)](https://codecov.io/github/while/FTRLProximal)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/FTRLProximal)](https://CRAN.R-project.org/package=FTRLProximal)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/FTRLProximal)](https://CRAN.R-project.org/package=FTRLProximal)

This is an R package of the FTRL Proximal algorithm for online learning of elastic net logistic regression models.

For more info on the algorithm please see [Ad Click Prediction: a View from the Trenches](https://www.eecs.tufts.edu/~dsculley/papers/ad-click-prediction.pdf) by McMahan et al. (2013).

## Installation
Easiest way to install is from within `R` using the latest CRAN version:

```r
install.packages("FTRLProximal")
```

If you want the latest build from git you can install it directly from github using `devtools`:

```r
devtools::install_github("while/FTRLProximal")
```

## Usage
Simplest use case is to use the model similar to normal glm with a model formula. 

```r
# Set up dataset
p <- mlbench::mlbench.2dnormals(100,2)
dat <- as.data.frame(p)

# Train model
mdl <- ftrlprox(classes ~ ., dat, lambda = 1e-2, alpha = 1, a = 0.3)

# Print resulting coeffs
print(mdl)
```

It is also possible to update the trained model object once it is trained.

```r
# Set up first dataset
p <- mlbench.2dnormals(100,2)
dat <- as.data.frame(p)

# Convert data.frame to model.matrix
X <- model.matrix(classes ~ ., dat)

# Train on first dataset
mdl <- ftrlprox(X, dat$classes, lambda = 1e-2, alpha = 1, a = 0.3)

# Generate more of the same data after the first training session
p <- mlbench.2dnormals(100,2)
dat <- as.data.frame(p)

# Update model using the new data.
mdl <- update(mdl, X, dat$classes)
```

For more example please see the documentation.

## Changelog
### 0.2
* Changed from using explicit `lambda1` and `lambda2` parameters to using `lambda` and mixing parameter `alpha`.
