# Run with valgrind like so:
# R -d valgrind --vanilla < norm2d.r

library(FTRLProximal)
library(mlbench)

p <- mlbench.2dnormals(1010,2)

dat <- data.frame(p$x)
dat$y <- factor(p$classes)

mdl <- ftrlprox(y ~ ., dat, alpha=1, beta=1,
                lambda1 = 0, lambda2 = 0)

coef(mdl)
