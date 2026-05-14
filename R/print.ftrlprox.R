##------------------------------------------------------------------------------
#' Print a ftrlprox model
#' 
#' Print a text representation of the ftrlprox model.
#' 
#' @param x The model object to print
#' @param digits the number of digits display in printout.
#' @param zero.print the symbol to use in place of zeros
#' @param ... additional args
#' @return Invisibly returns the model object \code{x}.
#'
#' @export
#' @method print ftrlprox
##------------------------------------------------------------------------------
print.ftrlprox <- function(x, digits=getOption("digits"), zero.print=".", ...) {
  rn <- names(x$theta)
  theta <- Matrix(x$theta, sparse=TRUE)
  rownames(theta) <- rn

  cat("\nCoefficients:")
  printSpMatrix(theta, digits=digits, zero.print=zero.print)
  cat("\n")
  invisible(x)
}

