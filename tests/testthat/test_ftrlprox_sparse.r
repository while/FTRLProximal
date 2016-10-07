library(mlbench)
context("Test ftrlprox using sparse model matrix")

set.seed(1)
p <- mlbench.2dnormals(100,2)

dat <- data.frame(x1=factor(p$x[ ,1]>=0),
                  x2=factor(p$x[ ,2]>=0))
dat$y <- factor(p$classes, labels=c("G", "B"))

X <- sparse.model.matrix(y ~ ., dat)

mdl <- ftrlprox(X, dat$y, a=1, b=1,
                lambda1 = 0, lambda2 = 0)

test_that("Class is ftrlprox", {
          expect_is(mdl, "ftrlprox")
})

test_that("Parameter values", {
          coefs <- mdl$theta
          names(coefs) <- NULL

          expect_equal(coefs[1],  1.3890292601737)
          expect_equal(coefs[2], -2.2134049722885)
          expect_equal(coefs[3], -1.8044648305589)
})

test_that("Parameter names", {
          expect_equal(names(mdl$theta), c("(Intercept)", "x1TRUE", "x2TRUE"))
})

test_that("Target levels", {
          expect_equal(mdl$levels, c("G", "B"))
})
