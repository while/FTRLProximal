library(mlbench)
context("Test ftrlprox using model matrix")

set.seed(1)
p <- mlbench.2dnormals(1000,2)

dat <- data.frame(p$x)
colnames(dat) <- c("A", "B")
dat$y <- factor(p$classes, labels=c("G", "B"))

X <- model.matrix(y ~ ., dat)

mdl <- ftrlprox(X, dat$y, lambda=0, alpha=1, a=0.3)

test_that("Class is ftrlprox", {
          expect_is(mdl, "ftrlprox")
})

test_that("Parameter values", {
          coefs <- mdl$theta
          names(coefs) <- NULL

          expect_equal(coefs[1], -0.0191484856739, tolerance=1e-8)
          expect_equal(coefs[2], -1.7695263198419, tolerance=1e-8)
          expect_equal(coefs[3], -1.6557359972582, tolerance=1e-8)
})

test_that("Parameter names", {
          expect_equal(names(mdl$theta), c("(Intercept)", "A", "B"))
})

test_that("Target levels", {
          expect_equal(mdl$levels, c("G", "B"))
})


test_that("Saving loss", {
          mdl <- ftrlprox(X, dat$y, a=0.3, b=1,
                          lambda=0, alpha=0,
                          save_loss=TRUE)

          expect_equal(length(mdl$J), nrow(X))
          expect_true(all(mdl$J != 0.0))
})

test_that("Saving loss many epochs", {
          mdl <- ftrlprox(X, dat$y, a=0.3, b=1,
                          lambda=1, alpha=1,
                          save_loss=TRUE, num_epochs=10)

          expect_equal(length(mdl$J), 10*nrow(X))
          expect_true(all(mdl$J != 0.0))
})


test_that("Different number of rows error", {
        expect_error(ftrlprox(X[1:2, ], dat$y[1:3], a=0.3, b=1, lambda=0, alpha=0),
                    "Input has differing number of rows, nrow(x)=2, length(y)=3",
                    fixed=TRUE) 
})


test_that("Parameter values for alpha=0.5", {
          coefs <- mdl$theta
          names(coefs) <- NULL

          expect_equal(coefs[1], -0.0191484856739, tolerance=1e-8)
          expect_equal(coefs[2], -1.7695263198419, tolerance=1e-8)
          expect_equal(coefs[3], -1.6557359972582, tolerance=1e-8)
})
