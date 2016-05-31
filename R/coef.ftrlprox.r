##------------------------------------------------------------------------------
#' coef.ftrlprox
#' 
#' Extract model coefficients.
#' 
#' This function can be used to extract the coefficients of a model trained using the ftrlprox package.
#' 
#' @param object The model object
#' @param ... additional arguments are not used.
#' @return an array with the regression coefficients
#' 
#' @author Vilhelm von Ehrenheim
#' @export
##------------------------------------------------------------------------------
coef.ftrlprox <- function(object, ...) {
  object$theta
}

