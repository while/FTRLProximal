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
#' @useDynLib FTRLProximal
#' @importFrom methods as
#' @export
##------------------------------------------------------------------------------
update.ftrlprox <- function(object, newX, newY, num_epochs=1, save_loss=F, ...) {
  if (!is.factor(newY))
    stop("Dependent variable must be a factor")

  if (nlevels(newY) != 2)
    stop("Dependent variable must be a factor with 2 levels")

  if (!all.equal(levels(newY), object$levels))
    stop("Dependent variable must have the same levels as original training data")

  # Make factor into numeric 0 and 1
  ynum <- as.numeric(newY) - 1  

  is_sparse <- FALSE
  ix <- jx <- NULL
  if (inherits(newX,"sparseMatrix")) {
    is_sparse <- TRUE
    newX <- as(newX,"CsparseMatrix")
    newX <- as(newX,"dgCMatrix")
  }

  J = if (save_loss) numeric(nrow(newX)*num_epochs) else numeric(0)

  out <- if(is_sparse) {
          .C("splognet_ftrlprox",
             X=as.double(newX@x),
             ix=as.integer(newX@p),
             jx=as.integer(newX@i),
             theta=object$theta,
             y=as.double(ynum),
             m=as.integer(nrow(newX)),
             n=as.integer(ncol(newX)),
             z=object$z,
             nn=object$nn,
             J=J,
             num_epochs=as.integer(num_epochs),
             a=as.double(object$a),
             b=as.double(object$b),
             lambda1=as.double(object$alpha*object$lambda),
             lambda2=as.double((1-object$alpha)*object$lambda),
             save_loss=as.integer(save_loss))
  } else {
          .C("lognet_ftrlprox",
             X=as.double(newX),
             theta=object$theta,
             y=as.double(ynum),
             m=as.integer(nrow(newX)),
             n=as.integer(ncol(newX)),
             z=object$z,
             nn=object$nn,
             J=J,
             num_epochs=as.integer(num_epochs),
             a=as.double(object$a),
             b=as.double(object$b),
             lambda1=as.double(object$alpha*object$lambda),
             lambda2=as.double((1-object$alpha)*object$lambda),
             save_loss=as.integer(save_loss))
  }

  # Append loss to old model objects loss
  out$J <- c(object$J, out$J)

  # Remove unnecessary from output
  out$X <- NULL
  out$y <- NULL
  out$m <- NULL
  out$n <- NULL
  out$save_loss <- NULL
  out$num_epochs <- NULL

  if (is_sparse) {
      out$ix <- NULL
      out$jx <- NULL
  }

  # Set the feature colnames as parameter names
  names(out$theta) <- colnames(newX)

  # Save target levels
  out$levels <- object$levels

  # Save regularization and mixing params instead of raw lambda values
  out$lambda <- object$lambda
  out$alpha  <- object$alpha

  class(out) <- "ftrlprox"
  out
}
