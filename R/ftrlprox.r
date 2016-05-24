##------------------------------------------------------------------------------
#' FTRL Proximal
#' 
#' Online elastic net regression using the FTRL Proximal algorithm for training.
#'
#' Test text
#'
#' @param formula modeling formula
#' @param data data.frame containing features and dependent variable
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
ftrlprox.formula <- function(formula, data, lambda1, lambda2, alpha, beta=1, num_epochs=1,
                   loss=F) {

  X <- model.matrix(formula, data)
  response <- all.vars(formula[[2]])

  if (!is.factor(data[[response]]))
    stop("Dependent variable must be a factor")

  if (nlevels(data[[response]]) != 2)
    stop("Dependent variable must be a factor with 2 levels")

  y <- as.numeric(data[[response]]) - 1  # Make factor into numeric 0 and 1

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

  out$fx <- formula
  out$levels <- levels(data[[response]])
  class(out) <- "ftrlprox"

  out
}



##------------------------------------------------------------------------------
#' @useDynLib FTRLProximal
#' @export
##------------------------------------------------------------------------------
ftrlprox <- function(x, y, lambda1, lambda2, alpha, beta=1, num_epochs=1,
                   loss=F, save.data=F) {
  if (!is.factor(y))
    stop("Dependent variable must be a factor")

  if (nlevels(y) != 2)
    stop("Dependent variable must be a factor with 2 levels")

  # Make factor into numeric 0 and 1
  ynum <- as.numeric(y) - 1  

  out <- .C("lognet_ftrlprox",
            X=as.double(x),
            theta=double(ncol(x)),
            y=as.double(ynum),
            m=as.integer(nrow(x)),
            n=as.integer(ncol(x)),
            J=numeric(num_epochs),
            num_epochs=as.integer(num_epochs),
            alpha=as.double(alpha),
            beta=as.double(beta),
            lambda1=as.double(lambda1),
            lambda2=as.double(lambda2),
            loss=as.integer(loss))

  if(!save.data) {
    # Remove dataset from output
    out$X <- NULL
    out$y <- NULL
  }

  # Set the feature colnames as parameter names
  names(out$theta) <- colnames(x)

  # Save target levels
  out$levels <- levels(y)

  class(out) <- "ftrlprox"
  out
}


