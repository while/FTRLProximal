library(mlbench)
context("Test ftrlprox using formula")

set.seed(1)
p <- mlbench.2dnormals(100,2)

dat <- data.frame(p$x)
colnames(dat) <- c("A", "B")
dat$y <- factor(p$classes, labels=c("G", "B"))

mdl <- ftrlprox(y ~ ., dat, alpha=1, beta=1,
                lambda1 = 0, lambda2 = 0)

test_that("Class is ftrlprox", {
          expect_is(mdl, "ftrlprox")
})

test_that("Correct parameters", {
          coefs <- mdl$theta
          names(coefs) <- NULL

          expect_equal(coefs[1], -0.38188318762577)
          expect_equal(coefs[2], -2.23889313859288)
          expect_equal(coefs[3], -1.69555552563667)
})

test_that("Correct parameter names", {
          expect_equal(names(mdl$theta), c("(Intercept)", "A", "B"))
})

test_that("Target levels", {
          expect_equal(mdl$levels, c("G", "B"))
})