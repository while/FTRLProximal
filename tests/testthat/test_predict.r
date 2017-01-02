library(mlbench)
context("Test ftrlprox prediction")

test_that("Predict on simple model", {
          mdl <- list(theta=c(1, 0.5, -0.5))
          class(mdl) <- "ftrlprox"

          expect_equal(predict(mdl, diag(3)),
                       c(1/(1+exp(-1)),
                         1/(1+exp(-0.5)),
                         1/(1+exp(0.5))))

          expect_equal(predict(mdl, diag(3), type="response"),
                       c(1/(1+exp(-1)),
                         1/(1+exp(-0.5)),
                         1/(1+exp(0.5))))
})


test_that("Predict class on simple model", {
          mdl <- list(theta=c(1, 0.5, -0.5), levels=letters[1:2])
          class(mdl) <- "ftrlprox"

          expect_equal(predict(mdl, diag(3), type="class"),
                       factor(c(2,2,1), labels = letters[1:2]))
})
