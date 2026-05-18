##------------------------------------------------------------------------------
#' Update FTRL Proximal model
#'
#' Continue training model on new data
#'
#' As FTRL PRoximal is an online algorithm it is possible to continue training the model on new data. This can be good if for for example the size of the dataset is too large to keep in memory or new data is getting available after some time.
#'
#' @param object the model object
#' @param newX new feature vectors. This needs to be the same features as used in previous training rounds for this object.
#' @param newY new observations
#' @param num_epochs number of times we should traverse over the training data, defaults to 1.
#' @param save_loss is to save the loss function during training. This will be appended to previous loss vector.
#' @param ... additional args
#' @return ftrlprox model object
#' @author Vilhelm von Ehrenheim
#'
#' @method update ftrlprox
#' @useDynLib FTRLProximal, .registration = TRUE
#' @importFrom methods as
#' @export
##------------------------------------------------------------------------------
update.ftrlprox <- function(object, newX, newY, num_epochs=1, save_loss=FALSE, ...) {
  .validate_response(newY, levels = object$levels)
  ynum <- as.numeric(newY) - 1
  newX <- .coerce_features(newX)

  out <- .fit_step(x = newX, ynum = ynum,
                   theta = object$theta, z = object$z, nn = object$nn,
                   lambda = object$lambda, alpha = object$alpha,
                   a = object$a, b = object$b,
                   num_epochs = num_epochs, save_loss = save_loss)

  out$J <- c(object$J, out$J)

  out$X <- NULL
  out$y <- NULL
  out$m <- NULL
  out$n <- NULL
  out$loss <- NULL
  out$num_epochs <- NULL

  if (inherits(newX, "dgCMatrix")) {
    out$ix <- NULL
    out$jx <- NULL
  }

  names(out$theta) <- colnames(newX)
  out$levels <- object$levels
  out$lambda <- object$lambda
  out$alpha  <- object$alpha

  class(out) <- "ftrlprox"
  out
}
