library(mlbench)
context("Test ftrlprox prediction")

test_that("Predict on simple model", {
          mdl <- list(theta=c(1, 0.5, -0.5))
          class(mdl) <- "ftrlprox"

          expect_equal(predict(mdl, diag(3)),
                       c(1/(1+exp(-1)),
                         1/(1+exp(-0.5)),
                         1/(1+exp(0.5))))
})
