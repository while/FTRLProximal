##------------------------------------------------------------------------------
#' FTRL Proximal for matrix class
#'
#' Online elastic net regression using the FTRL Proximal algorithm for training.
#'
#' This method is intended for matrix input.
#'
#' @param x the model matrix containing features
#' @param y the response variable
#' @param lambda regularization term
#' @param alpha mixing parameter, alpha=0 corresponds to L2 regularization and alpha=1 to L1.
#' @param a learning rate parameter.
#' @param b learning rate parameter controlling decay, defaults to 1.
#' @param num_epochs number of times we should traverse over the traiing set, defaults to 1.
#' @param save_loss is to save the loss function during training.
#' @param ... additional args
#' @return ftrlprox model object
#' @author Vilhelm von Ehrenheim
#'
#' @method ftrlprox default
#' @useDynLib FTRLProximal, .registration = TRUE
#' @importFrom methods as
#' @export
##------------------------------------------------------------------------------
ftrlprox.default <- function(x, y, lambda, alpha, a, b=1, num_epochs=1,
                             save_loss=FALSE, ...) {
  if (nrow(x) != length(y))
    stop(sprintf("Input has differing number of rows, nrow(x)=%d, length(y)=%d",
                 nrow(x), length(y)))

  .validate_response(y)
  ynum <- as.numeric(y) - 1
  x <- .coerce_features(x)
  n <- ncol(x)

  out <- .fit_step(x = x, ynum = ynum,
                   theta = numeric(n), z = numeric(n), nn = numeric(n),
                   lambda = lambda, alpha = alpha, a = a, b = b,
                   num_epochs = num_epochs, save_loss = save_loss)

  out$X <- NULL
  out$y <- NULL
  if (inherits(x, "dgCMatrix")) {
    out$ix <- NULL
    out$jx <- NULL
  }

  out$lambda <- lambda
  out$alpha  <- alpha
  names(out$theta) <- colnames(x)
  out$levels <- levels(y)

  class(out) <- "ftrlprox"
  out
}
