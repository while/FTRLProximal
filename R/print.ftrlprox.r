##------------------------------------------------------------------------------
#' Print a ftrlprox model
#' 
#' Print a text represenation of the ftrlprox model.
#' 
#' @param obj The model object to print
#' @param digits the number of digits display in printout.
#' @param zero.print the symbol to use in place of zeros
#' 
#' @author Vilhelm von Ehrenheim
#' @import Matrix
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
}

