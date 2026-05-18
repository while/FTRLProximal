#' FTRLProximal: FTRL Proximal Implementation for Elastic Net Regression
#'
#' Implementation of the Follow The Regularized Leader (FTRL) Proximal
#' algorithm, proposed by McMahan et al. (2013)
#' \doi{10.1145/2487575.2488200}, used for online training of large scale
#' regression models using a mixture of L1 and L2 regularization.
#'
#' @author Vilhelm von Ehrenheim
#' @keywords internal
#' @useDynLib FTRLProximal, .registration = TRUE
#' @import Matrix
#' @importFrom methods as
#' @importFrom stats model.matrix
"_PACKAGE"
