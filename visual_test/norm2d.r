vcontext("norm2d")

require(mlbench)
require(RColorBrewer)
require(foreach)
require(ggplot2)
set.seed(1)
m <- 1000
p <- mlbench.2dnormals(m,2)

dat <- data.frame(p$x)
dat$y <- p$classes

# colors
pal <- brewer.pal(5,"Set1")

ns <- c(1,10,30,100,300)


bounds <- foreach(i=1:5, .combine=rbind, .multicombine=T) %do% { 
  mdl1 <- ftrlprox(y ~ ., dat,
                   alpha=3.6, beta=1,
                   lambda1=0.0, lambda2=0.0,
                   num_epochs=ns[i])

  data.frame(k=-mdl1$theta[1]/mdl1$theta[3],
             m=-mdl1$theta[2]/mdl1$theta[3],
             lab=paste(ns[i], "epochs"))
}

# Train standard glm model for comparison
mdl2 <- glm(y ~ ., dat, family=binomial(link="logit"))
fits <- rbind(bounds, 
              data.frame(k=-mdl2$coefficients[1]/mdl2$coefficients[3],
                         m=-mdl2$coefficients[2]/mdl2$coefficients[3],
                         lab="glm"))

ggplot(dat, aes(x=X1, y=X2, color=y)) + geom_point(pch=1)
save_vtest("norm2d with different nbr of epochs")

end_vcontext()
