# FTRL Proximal
[![Build Status](https://travis-ci.org/while/FTRLProximal.svg?branch=master)](https://travis-ci.org/while/FTRLProximal)

This is an R package of the FTRL Proximal algorithm for online learning.

For more info on the algorithm please see [ad-click-prediction.pdf](https://www.eecs.tufts.edu/~dsculley/papers/ad-click-prediction.pdf)

## Installation
So for this package is not on CRAN. Easiest way to install it is using `devtools`.

```r
devtools::install_github("while/FTRLProximal")
```

## Usage
Simplest use case is to use the model similar to normal glm with a model formula. 

```r
# Set up dataset
set.seed(1)
p <- mlbench::mlbench.2dnormals(100,2)
dat <- as.data.frame(p)

# Train model
mdl <- ftrlprox(classes ~ ., dat, alpha=1, beta=1,
                lambda1 = 0, lambda2 = 0)

# Print resulting coeffs
print(mdl)
```

It is also possible to update the trained model object once it is trained.

```r
# Set up first dataset
set.seed(1)
p <- mlbench.2dnormals(100,2)
dat <- as.data.frame(p)

# Convert data.frame to model.matrix
X <- model.matrix(classes ~ ., dat)

# Train on first half of dataset
mdl <- ftrlprox(X, dat$classes, alpha=1, beta=1, lambda1=0, lambda2=0)

# Generate more of the same data after the first training session
set.seed(2)
p <- mlbench.2dnormals(100,2)
dat <- as.data.frame(p)

# Update model using the new data.
mdl <- update(mdl, X, dat$classes)
```

For more example please see the documentation.
