library(mlbench)
context("Test ftrlprox print")

set.seed(1)
p <- mlbench.2dnormals(1000,2)
dat <- as.data.frame(p)
dat$x.3 <- rnorm(100)

mdl <- ftrlprox(classes ~ ., dat, a=0.3, lambda = 1, alpha = 1)

test_that("Default print", {
          # Set default number of digits
          options(digits=6)

          # Test default amount of digits
          expect_output(print(mdl),
                        "Coefficients:\\s+\\(Intercept\\)\\s+-0.00677584\\s+x\\.1\\s+-1.75985861\\s+x\\.2\\s+-1.64894623\\s+x\\.3\\s+\\.")
})


test_that("Print 3 digits", {
          # Test 3 digits
          expect_output(print(mdl, digits=3),
                        "Coefficients:\\s+\\(Intercept\\)\\s+-0.00678\\s+x\\.1\\s+-1.75986\\s+x\\.2\\s+-1.64895\\s+x\\.3\\s+\\.")
})

test_that("Print 3 digits and ", {
          # Test 3 digits
          expect_output(print(mdl, digits=3, zero.print="z"),
                        "Coefficients:\\s+\\(Intercept\\)\\s+-0.00678\\s+x\\.1\\s+-1.75986\\s+x\\.2\\s+-1.64895\\s+x\\.3\\s+z")
})
