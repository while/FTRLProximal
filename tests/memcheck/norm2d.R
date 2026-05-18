# Run with valgrind like so:
# R -d valgrind --vanilla < norm2d.r

library(FTRLProximal)
library(mlbench)

p <- mlbench.2dnormals(1010,2)

dat <- data.frame(p$x)
dat$y <- factor(p$classes)

mdl <- ftrlprox(y ~ ., dat, a = 0.3, b = 1,
                lambda = 0, alpha = 1)

coef(mdl)
