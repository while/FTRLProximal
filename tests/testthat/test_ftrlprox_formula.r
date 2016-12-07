library(mlbench)
context("Test ftrlprox using formula")

set.seed(1)
p <- mlbench.2dnormals(100,2)
dat <- as.data.frame(p)

mdl <- ftrlprox(classes ~ ., dat, a = 0.3, lambda = 0, alpha = 1)

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


# -----------------------------------------------------------------------------
test_that("Params are 0 for large lambda", {
          mdl <- ftrlprox(classes ~ ., dat, a = 0.3, lambda = 100, alpha = 1)
          coefs <- mdl$theta
          names(coefs) <- NULL

          expect_equal(coefs[1], 0.0)
          expect_equal(coefs[2], 0.0)
          expect_equal(coefs[3], 0.0)
})


# -----------------------------------------------------------------------------
test_that("Params are 0 for random factor using some L1 reg", {
          set.seed(1)
          dat2 <- dat
          dat2$random_alpha <- factor(sample(letters, nrow(dat), T), levels=letters)
          mdl <- ftrlprox(classes ~ ., dat2, a = 0.3, lambda = 2, alpha = 1)
          coefs <- coef(mdl)

          expect_equal(coefs[[1]], 0.0)
          expect_equal(coefs[[2]], -1.15539216945350, tolerance=1e-8)
          expect_equal(coefs[[3]], -1.09002332655129, tolerance=1e-8)
          expect_equal(coefs[[4]], 0.0)
          expect_equal(coefs[[5]], 0.0)
          expect_equal(coefs[[6]], 0.0)
          expect_equal(coefs[[7]], 0.0)
          expect_equal(coefs[[8]], 0.0)
          expect_equal(coefs[[9]], 0.0)
          expect_equal(coefs[[9]], 0.0)
          expect_equal(coefs[[10]], 0.0)
          expect_equal(coefs[[11]], 0.0)
          expect_equal(coefs[[12]], 0.0)
          expect_equal(coefs[[13]], 0.0)
          expect_equal(coefs[[14]], 0.0)
          expect_equal(coefs[[15]], 0.0)
          expect_equal(coefs[[16]], 0.0)
          expect_equal(coefs[[17]], 0.0)
          expect_equal(coefs[[18]], 0.0)
          expect_equal(coefs[[19]], 0.0)
          expect_equal(coefs[[20]], 0.0)
          expect_equal(coefs[[21]], 0.0)
          expect_equal(coefs[[22]], 0.0)
          expect_equal(coefs[[23]], 0.0)
          expect_equal(coefs[[24]], 0.0)
          expect_equal(coefs[[25]], 0.0)
          expect_equal(coefs[[26]], 0.0)
          expect_equal(coefs[[27]], 0.0)
          expect_equal(coefs[[28]], 0.0)
})
