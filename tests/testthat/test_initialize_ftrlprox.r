library(mlbench)
context("Test initialize and update ftrlprox")

set.seed(1)
p <- mlbench.2dnormals(100,2)

dat <- data.frame(p$x)
colnames(dat) <- c("A", "B")
dat$y <- factor(p$classes, labels=c("G", "B"))

X <- model.matrix(y ~ ., dat)

theta <- numeric(3)
names(theta) <- c("(Intercept)", "A", "B")

# Set up enpty object
mdl <- initialize.ftrlprox(theta, c("G", "B"), a=0.3, b=1, lambda=0, alpha=0)


test_that("Class is ftrlprox", {
          expect_is(mdl, "ftrlprox")
})

test_that("Parameter values", {
          coefs <- mdl$theta
          expected <- c('(Intercept)' = 0.0, 'A' = 0.0, 'B' = 0.0)

          expect_equal(coefs, expected, tolerance=1e-8)
})


test_that("Parameter values after update", {
          # Update model using the rest of the data this should generate the same result
          # as training once using all data.
          mdl <- update(mdl, X, dat$y)

          coefs <- mdl$theta
          expected <- c('(Intercept)' = -0.110366358105649,
                        'A' = -1.303382372935719,
                        'B' = -1.169874403463117)

          expect_equal(coefs, expected, tolerance=1e-8)
})

test_that("Parameter names", {
          expect_equal(names(mdl$theta), c("(Intercept)", "A", "B"))
})

test_that("Target levels", {
          expect_equal(mdl$levels, c("G", "B"))
})

