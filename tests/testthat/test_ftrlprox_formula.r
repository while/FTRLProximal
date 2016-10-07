library(mlbench)
context("Test ftrlprox using formula")

set.seed(1)
p <- mlbench.2dnormals(100,2)
dat <- as.data.frame(p)

mdl <- ftrlprox(classes ~ ., dat, a=1, b=1,
                lambda1 = 0, lambda2 = 0)

test_that("Class is ftrlprox", {
          expect_is(mdl, "ftrlprox")
})

test_that("Parameter values", {
          coefs <- mdl$theta
          names(coefs) <- NULL

          expect_equal(coefs[1], -0.38188318762577)
          expect_equal(coefs[2], -2.23889313859288)
          expect_equal(coefs[3], -1.69555552563667)
})

test_that("Parameter names", {
          expect_equal(names(mdl$theta), c("(Intercept)", "x.1", "x.2"))
})

test_that("Target levels", {
          expect_equal(mdl$levels, c("1", "2"))
})
