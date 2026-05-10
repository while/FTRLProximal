##------------------------------------------------------------------------------
#' Print a ftrlprox model
#' 
#' Print a text represenation of the ftrlprox model.
#' 
#' @param x The model object to print
#' @param digits the number of digits display in printout.
#' @param zero.print the symbol to use in place of zeros
#' @param ... additional args
#' @return Invisibly returns the model object \code{x}.
#'
#' @author Vilhelm von Ehrenheim
#' @import Matrix
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

