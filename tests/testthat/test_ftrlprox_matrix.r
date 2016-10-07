library(mlbench)
context("Test ftrlprox using model matrix")

set.seed(1)
p <- mlbench.2dnormals(100,2)

dat <- data.frame(p$x)
colnames(dat) <- c("A", "B")
dat$y <- factor(p$classes, labels=c("G", "B"))

X <- model.matrix(y ~ ., dat)

mdl <- ftrlprox(X, dat$y, a=1, b=1,
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
          expect_equal(names(mdl$theta), c("(Intercept)", "A", "B"))
})

test_that("Target levels", {
          expect_equal(mdl$levels, c("G", "B"))
})


test_that("Saving loss", {
          mdl <- ftrlprox(X, dat$y, a=1, b=1,
                          lambda1=0, lambda2=0,
                          save_loss=TRUE)

          expect_equal(length(mdl$J), nrow(X))
          expect_true(all(mdl$J != 0.0))
})

test_that("Saving loss many epochs", {
          mdl <- ftrlprox(X, dat$y, a=1, b=1,
                          lambda1=0, lambda2=0,
                          save_loss=TRUE, num_epochs=10)

          expect_equal(length(mdl$J), 10*nrow(X))
          expect_true(all(mdl$J != 0.0))
})
