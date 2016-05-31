##------------------------------------------------------------------------------
#' FTRL Proximal formula
#' 
#' Online elastic net regression using the FTRL Proximal algorithm for training.
#'
#' Test text
#'
#' @param formula modeling formula
#' @param data data.frame containing features and dependent variable
#' @param lambda1 L1 regularization term
#' @param lambda2 L2 regularization term
#' @param alpha learning rate parameter
#' @param beta learning rate parameter controlling decay, defaults to 1.
#' @param num_epochs number of times we should traverse over the traiing set, defaults to 1.
#' @return ftrlprox model object
#' @author Vilhelm von Ehrenheim
#' @useDynLib FTRLProximal
#' @export
#' 
#' @examples
#' require(mlbench)
#' 
#' p <- mlbench.circle(100,2)
#' dat <- data.frame(p$x)
#' dat$y <- factor(p$classes)
#' 
#' mdl <- ftrlprox(y ~ X1 + X2 + I(X1^2) + I(X2^2), dat,
#'                 alpha=1, lambda1=5.0, lambda2=0.0)
#' print(mdl)
##------------------------------------------------------------------------------
ftrlprox.formula <- function(formula, data, lambda1, lambda2, alpha, beta=1, num_epochs=1, loss=F) {

  X <- model.matrix(formula, data)
  y <- data[[all.vars(formula[[2]])]]

  ftrlprox(X,y,alpha=alpha,beta=beta,
           lambda1=lambda1, lambda2=lambda2,
           num_epochs=num_epochs, loss=loss)
}

