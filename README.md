# FTRL Proximal
[![Build Status](https://travis-ci.org/while/FTRLProximal.svg?branch=master)](https://travis-ci.org/while/FTRLProximal)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/FTRLProximal)](http://CRAN.R-project.org/package=FTRLProximal)

This is an R package of the FTRL Proximal algorithm for online learning.

For more info on the algorithm please see [Ad Click Prediction: a View from the Trenches](https://www.eecs.tufts.edu/~dsculley/papers/ad-click-prediction.pdf)

## Installation
Easiest way to install it is from within `R` using ieither CRAN version or directly from github using `devtools`.

```r
install.packages("FTRLProximal")
```

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
mdl <- ftrlprox(classes ~ ., dat, a=1, b=1, lambda1 = 0, lambda2 = 0)

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
mdl <- ftrlprox(X, dat$classes, a=1, b=1, lambda1=0, lambda2=0)

# Generate more of the same data after the first training session
p <- mlbench.2dnormals(100,2)
dat <- as.data.frame(p)

# Update model using the new data.
mdl <- update(mdl, X, dat$classes)
```

For more example please see the documentation.
