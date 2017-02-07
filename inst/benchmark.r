#-------------------------------------------------------------------------------
#
# Benchmarks vs glmnet on different training and predictions tasks
#
#-------------------------------------------------------------------------------
library(mlbench)
library(microbenchmark)
library(ggplot2)
library(glmnet)

#-------------------------------------------------------------------------------
# Benchmark default training function on dense 100d dataset
#-------------------------------------------------------------------------------
dat <- as.data.frame(mlbench.threenorm(1000, d=100))
X <- model.matrix(classes ~ ., dat)
y <- as.numeric(dat$classes) - 1

tm <- microbenchmark(ftrlprox(X, dat$classes, a=0.3, lambda=0, alpha=1),
                     glmnet(X, y, lambda=0, alpha=1, family="binomial"),
                     times = 100L)

autoplot(tm) + geom_boxplot(width=0.1, fill='steelblue', alpha=0.5)
print(tm)


#-------------------------------------------------------------------------------
# Benchmark different training functions on dense 100d dataset
#-------------------------------------------------------------------------------
dat <- as.data.frame(mlbench.threenorm(1000, d=100))
X <- model.matrix(classes ~ ., dat)
y <- as.numeric(dat$classes) - 1

theta <- numeric(ncol(X))
names(theta) <- colnames(X)
mdl <- initialize.ftrlprox(theta, levels(dat$classes), a=0.3, lambda=0, alpha=1)

tm <- microbenchmark(ftrlprox(X, dat$classes, a=0.3, lambda=0, alpha=1),
                     ftrlprox(classes ~ ., dat, a=0.3, lambda=0, alpha=1),
                     glmnet(X, y, lambda=0, alpha=1, family="binomial"),
                     update(mdl, X, dat$classes),
                     times = 100L)

autoplot(tm) + geom_boxplot(width=0.1, fill='steelblue', alpha=0.5)
print(tm)


#-------------------------------------------------------------------------------
# Benchmark default training function on dense short and wide set
#-------------------------------------------------------------------------------
dat <- as.data.frame(mlbench.threenorm(100, d=1000))
X <- model.matrix(classes ~ ., dat)
y <- as.numeric(dat$classes) - 1

tm <- microbenchmark(ftrlprox(X, dat$classes, a=0.3, lambda=0, alpha=1),
                     glmnet(X, y, lambda=0, alpha=1, family="binomial"),
                     times = 100L)

autoplot(tm) + geom_boxplot(width=0.1, fill='steelblue', alpha=0.5)
print(tm)


#-------------------------------------------------------------------------------
# Benchmark default training function on dense long and slim dataset
#-------------------------------------------------------------------------------
dat <- as.data.frame(mlbench.threenorm(100000, d=10))
X <- model.matrix(classes ~ ., dat)
y <- as.numeric(dat$classes) - 1

tm <- microbenchmark(ftrlprox(X, dat$classes, a=0.3, lambda=0, alpha=1),
                     glmnet(X, y, lambda=0, alpha=1, family="binomial"),
                     times = 100L, control = list(warmup = 10))

autoplot(tm) + geom_boxplot(width=0.1, fill='steelblue', alpha=0.5)
print(tm)


#-------------------------------------------------------------------------------
# Benchmark predict function on dense long and slim dataset
#-------------------------------------------------------------------------------
dat <- as.data.frame(mlbench.threenorm(100000, d=10))
X <- model.matrix(classes ~ ., dat)
y <- as.numeric(dat$classes) - 1

ftrlprox_mdl <- ftrlprox(X, dat$classes, a=0.3, lambda=0, alpha=1)
glmnet_mdl <- glmnet(X, y, lambda=0, alpha=1, family="binomial")

dat <- as.data.frame(mlbench.threenorm(100000, d=10))
newX <- model.matrix(classes ~ ., dat)

tm <- microbenchmark(predict(ftrlprox_mdl, newX),
                     predict(glmnet_mdl, newX),
                     times = 1000L)

autoplot(tm) + geom_boxplot(width=0.1, fill='steelblue', alpha=0.5)
print(tm)


#-------------------------------------------------------------------------------
# Benchmark default training function on sparse vs non sparse model mtx
#-------------------------------------------------------------------------------
dat <- as.data.frame(mlbench.threenorm(10000, d=100))

#for(d in paste0('x.', 1:100)) {
#   q <- quantile(dat[ , d], seq(0,1,length.out=6))
#   dat[ ,d] <- cut(dat[ ,d], breaks = q, include.lowest=T)
#}

Xsp <- sparse.model.matrix(classes ~ ., dat)
X <- model.matrix(classes ~ ., dat)
y <- as.numeric(dat$classes) - 1

cat(sprintf("Dim: %d x %d\nShare non-zero values: %.5f\n",
            nrow(Xsp), ncol(Xsp), length(Xsp@i)/prod(Xsp@Dim)))

tm <- microbenchmark(ftrlprox(Xsp, dat$classes, a=0.3, lambda=0, alpha=1),
                     glmnet(Xsp, y, lambda=0, alpha=1, family="binomial"),
                     ftrlprox(X, dat$classes, a=0.3, lambda=0, alpha=1),
                     glmnet(X, y, lambda=0, alpha=1, family="binomial"),
                     times = 100L)

print(tm)
autoplot(tm) + geom_boxplot(width=0.1, fill='steelblue', alpha=0.5)
