##------------------------------------------------------------------------------
#' FTRL Proximal
#' 
#' Online elastic net regression using the FTRL Proximal algorithm for training.
#'
#' This is the generic method. Please look at ftrlprox.matrix and ftrlprox.formula for the model matrix and formula versions respectively.
#'
#' @param x the model matrix containing features
#' @param ... the rest of the model parameters
#' @return ftrlprox model object
#' @author Vilhelm von Ehrenheim
#'
#' @useDynLib FTRLProximal
#' @export
##------------------------------------------------------------------------------
ftrlprox <- function(x, ...) {
  UseMethod("ftrlprox")
}
