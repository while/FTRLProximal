library(mlbench)
context("Test ftrlprox using sparse model matrix")

set.seed(1)
p <- mlbench.2dnormals(100,2)

dat <- data.frame(x1=factor(p$x[ ,1]>=0),
                  x2=factor(p$x[ ,2]>=0))
dat$y <- factor(p$classes, labels=c("G", "B"))

X <- sparse.model.matrix(y ~ ., dat)

mdl <- ftrlprox(X, dat$y, a = 0.3, lambda = 0, alpha = 1)

test_that("Class is ftrlprox", {
          expect_is(mdl, "ftrlprox")
})

test_that("Parameter values", {
          coefs <- mdl$theta
          names(coefs) <- NULL

          expect_equal(coefs[1], 0.431587946046618, tolerance=1e-8)
          expect_equal(coefs[2], -1.036847034291526, tolerance=1e-8)
          expect_equal(coefs[3], -0.918381020470813, tolerance=1e-8)
})

test_that("Parameter names", {
          expect_equal(names(mdl$theta), c("(Intercept)", "x1TRUE", "x2TRUE"))
})

test_that("Target levels", {
          expect_equal(mdl$levels, c("G", "B"))
})
