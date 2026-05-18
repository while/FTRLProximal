##------------------------------------------------------------------------------
#' Initialize an empty FTRL Proximal model object
#'
#' Construct a fresh, untrained \code{ftrlprox} model object so it can be
#' trained incrementally via \code{\link{update.ftrlprox}}. Useful when the
#' training set is too large to fit in memory and must be streamed in chunks.
#'
#' @param theta named numeric containing initial coefficients
#' @param levels character vector containing class labels of target label
#' @param lambda regularization term
#' @param alpha mixing parameter, alpha=0 corresponds to L2 regularization and alpha=1 to L1.
#' @param a learning rate parameter
#' @param b learning rate parameter controlling decay, defaults to 1.
#' @param save_loss is to save the loss function during training.
#' @param ... additional args
#' @return ftrlprox model object
#' @author Vilhelm von Ehrenheim
#'
#' @useDynLib FTRLProximal, .registration = TRUE
#' @export
##------------------------------------------------------------------------------
ftrlprox_init <- function(theta, levels, lambda, alpha, a, b = 1,
                          save_loss = FALSE, ...) {

  n <- length(theta)

  out <- list(theta = theta,
              m = 0,
              n = n,
              z = numeric(n),
              nn = numeric(n),
              J = numeric(0),
              num_epochs = 1,
              a = a,
              b = b,
              lambda = lambda,
              alpha = alpha,
              loss = integer(1),
              levels = levels)

  class(out) <- "ftrlprox"
  out
}


##------------------------------------------------------------------------------
#' Initialize an empty FTRL Proximal model object (deprecated)
#'
#' @description
#' \strong{Deprecated.} This function has been renamed to
#' \code{\link{ftrlprox_init}} to avoid clashing with the S4 generic
#' \code{methods::initialize}. Please use \code{ftrlprox_init} instead.
#'
#' @inheritParams ftrlprox_init
#' @return ftrlprox model object
#' @author Vilhelm von Ehrenheim
#'
#' @seealso \code{\link{ftrlprox_init}}
#' @export
##------------------------------------------------------------------------------
initialize.ftrlprox <- function(theta, levels, lambda, alpha, a, b = 1,
                                save_loss = FALSE, ...) {
  .Deprecated("ftrlprox_init")
  ftrlprox_init(theta = theta, levels = levels, lambda = lambda, alpha = alpha,
                a = a, b = b, save_loss = save_loss, ...)
}
