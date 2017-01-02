library(mlbench)
context("Test update ftrlprox")

set.seed(1)
p <- mlbench.2dnormals(100,2)
dat <- as.data.frame(p)

X <- model.matrix(classes ~ ., dat)

# Train on first half of dataset
mdl <- ftrlprox(X[1:50, ], dat$classes[1:50], a = 0.3, lambda=0, alpha=0)

# Update model using the rest of the data this should generate the same result
# as training once using all data.
mdl <- update(mdl, X[51:100, ], dat$classes[51:100])

test_that("Class is ftrlprox", {
          expect_is(mdl, "ftrlprox")
})

test_that("Parameter values", {
          coefs <- mdl$theta
          names(coefs) <- NULL

          expect_equal(coefs[1], -0.110366358105649, tolerance=1e-8)
          expect_equal(coefs[2], -1.303382372935719, tolerance=1e-8)
          expect_equal(coefs[3], -1.169874403463117, tolerance=1e-8)
})

test_that("Parameter names", {
          expect_equal(names(mdl$theta), c("(Intercept)", "x.1", "x.2"))
})

test_that("Target levels", {
          expect_equal(mdl$levels, c("1", "2"))
})


test_that("Saving loss", {
          mdl <- ftrlprox(X[1:50, ], dat$classes[1:50], a = 0.3,
                          lambda=0, alpha=0, save_loss=TRUE)
          mdl <- update(mdl, X[51:100, ], dat$classes[51:100], save_loss=TRUE)

          expect_equal(length(mdl$J), nrow(X))
          expect_true(all(mdl$J != 0.0))
})

test_that("Saving loss many epochs", {
          mdl <- ftrlprox(X[1:50, ], dat$classes[1:50], a=0.3,
                          lambda=0, alpha=0, save_loss=TRUE, num_epochs=10)
          mdl <- update(mdl, X[51:100, ], dat$classes[51:100],
                        save_loss=TRUE, num_epochs=10)

          expect_equal(length(mdl$J), 10*nrow(X))
          expect_true(all(mdl$J != 0.0))
})


test_that("Not a factor error", {
        expect_error(ftrlprox(X, as.numeric(dat$classes),
                              lambda=0, alpha=0, a=0.3),
                     "Dependent variable must be a factor")
})


test_that("Not a factor w 2 levels", {
        expect_error(ftrlprox(X, factor(dat$classes, levels=1:4),
                              lambda=0, alpha=0, a=0.3),
                     "Dependent variable must be a factor with 2 levels")
})


test_that("Parameter values trained on sparse matrix", {
          spX <- Matrix::sparse.model.matrix(classes ~ ., dat)

          # Train on first half of dataset
          mdl <- ftrlprox(spX[1:50, ], dat$classes[1:50], a = 0.3, lambda=0, alpha=0)

          # Update model using the rest of the data this should generate the same result
          # as training once using all data.
          mdl <- update(mdl, spX[51:100, ], dat$classes[51:100])

          coefs <- mdl$theta
          expected <- c('(Intercept)'=-0.110366358105649,
                        'x.1' = -1.303382372935719,
                        'x.2' = -1.169874403463117)

          expect_equal(coefs, expected, tolerance=1e-8)
})

