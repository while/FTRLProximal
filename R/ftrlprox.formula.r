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
#' @param a learning rate parameter
#' @param b learning rate parameter controlling decay, defaults to 1.
#' @param num_epochs number of times we should traverse over the traiing set, defaults to 1.
#' @param save_loss is to save the loss function during training.
#' @param ... additional args
#' @return ftrlprox model object
#' @author Vilhelm von Ehrenheim
#' @useDynLib FTRLProximal
#' @importFrom stats model.matrix
#' @export
#' 
#' @examples
#' require(mlbench)
#' 
#' p <- mlbench.circle(100,2)
#' dat <- as.data.frame(p)
#' 
#' mdl <- ftrlprox(classes ~ x.1 + x.2 + I(x.1^2) + I(x.2^2), dat,
#'                 a=1, b=1, lambda1=5.0, lambda2=0.0)
#' print(mdl)
##------------------------------------------------------------------------------
ftrlprox.formula <- function(formula, data, lambda1, lambda2, a, b=1, num_epochs=1, save_loss=F, ...) {

  X <- model.matrix(formula, data)
  y <- data[[all.vars(formula[[2]])]]

  ftrlprox(X,y,a=a,b=b,
           lambda1=lambda1, lambda2=lambda2,
           num_epochs=num_epochs, save_loss=save_loss)
}

