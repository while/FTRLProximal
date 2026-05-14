##------------------------------------------------------------------------------
#' coef.ftrlprox
#' 
#' Extract model coefficients.
#' 
#' This function can be used to extract the coefficients of a model trained using \code{\link{ftrlprox}}.
#' 
#' @param object The model object
#' @param ... additional arguments are not used.
#' @return an array with the regression coefficients
#'
#' @export
##------------------------------------------------------------------------------
coef.ftrlprox <- function(object, ...) {
  object$theta
}

