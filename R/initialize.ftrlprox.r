##------------------------------------------------------------------------------
#' Initialize empty FTRL Proximal class
#' 
#' Online elastic net regression using the FTRL Proximal algorithm for training.
#'
#' This method is intended for setting up a ftrlprox model object before training it using update. 
#'
#' @param theta named numeric containing initial coefficients
#' @param levels character vector containing class labels of target label
#' @param lambda1 L1 regularization term
#' @param lambda2 L2 regularization term
#' @param a learning rate parameter
#' @param b learning rate parameter controlling decay, defaults to 1.
#' @param save_loss is to save the loss function during training.
#' @param ... additional args
#' @return ftrlprox model object
#' @author Vilhelm von Ehrenheim
#'
#' @useDynLib FTRLProximal
#' @export
##------------------------------------------------------------------------------
initialize.ftrlprox <- function(theta, levels, lambda1, lambda2, a, b=1, save_loss=F, ...) {

  n <- length(theta)

  # Set up an empty object
  out <- list(theta = theta,
              m = 0,
              n = n,
              z = numeric(n),
              nn = numeric(n),
              J = numeric(0),
              num_epochs = 1,
              a = a,
              b = b,
              lambda1 = lambda1,
              lambda2 = lambda2,
              loss = integer(1),
              levels = levels)

  class(out) <- "ftrlprox"
  out
}

