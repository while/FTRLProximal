##------------------------------------------------------------------------------
#' Elastic net regression
#'
#' @param fx modeling formula
#' @param dat data.frame containing features and dependent variable
#' @param lambda1 L1 regularization term
#' @param lambda2 L2 regularization term
#' @param alpha main learning rate parameter
#' @param beta parameter controlling learning rate at an early stage, defaults to 1.
#' @param num_epochs number of times we should traverse over the traiing set, defaults to 1.
#' @return ftrlprox model object
#' @author Vilhelm von Ehrenheim
#'
#' @useDynLib FTRLProximal
#' @export
##------------------------------------------------------------------------------
ftrlprox <- function(fx, dat, lambda1, lambda2, alpha, beta=1, num_epochs=1,
                   loss=F) {

  X <- model.matrix(fx, dat)
  dep_var <- all.vars(fx[[2]])

  if (!is.factor(dat[[dep_var]]))
    stop("Dependent variable must be a factor")

  if (nlevels(dat[[dep_var]]) != 2)
    stop("Dependent variable must be a factor with 2 levels")

  y <- as.numeric(dat[[dep_var]]) - 1  # Make factor into numeric 0 and 1

  out <- .C("lognet_ftrlprox",
            X=as.double(X),
            theta=double(ncol(X)),
            y=as.double(y),
            m=as.integer(nrow(X)),
            n=as.integer(ncol(X)),
            J=numeric(num_epochs),
            num_epochs=as.integer(num_epochs),
            alpha=as.double(alpha),
            beta=as.double(beta),
            lambda1=as.double(lambda1),
            lambda2=as.double(lambda2),
            loss=as.integer(loss))

  names(out$theta) <- colnames(X)

  out$fx <- fx
  out$levels <- levels(dat[[dep_var]])
  class(out) <- "ftrlprox"

  p <- predict(out, newdata=dat)
  p_null <- rep(mean(y), nrow(X))

  llik <- sum(ifelse(y==1, log(p), log(1-p)))

  k <- sum(out$theta > 0) 
  out$aic <- round(2*k - 2*llik)

  return(out)
}


##------------------------------------------------------------------------------
#' @export
##------------------------------------------------------------------------------
predict.ftrlprox <- function(obj, newdata=NULL, type="response") {
  dep_var <- all.vars(obj$fx[[2]])
  newdata[[dep_var]] <- 0

  X <- model.matrix(obj$fx, newdata)

  out <- .C("lognet_predict",
            X=as.double(X),
            theta=as.double(obj$theta),
            y=numeric(nrow(X)),
            m=as.integer(nrow(X)),
            n=as.integer(ncol(X)))

  return(out$y)
}

##------------------------------------------------------------------------------
#' @export
##------------------------------------------------------------------------------
print.ftrlprox <- function(obj, digits=NULL, zero.print=".") {
  require(Matrix)
  rn <- names(obj$theta)
  theta <- Matrix(obj$theta, sparse=T)
  rownames(theta) <- rn

  cat("\nCoefficients:")
  printSpMatrix(theta, digits=digits, zero.print=zero.print)
  cat("\n")
  cat(sprintf("AIC: %d\n", obj$aic))
}
