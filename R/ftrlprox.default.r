##------------------------------------------------------------------------------
#' FTRL Proximal for matrix class
#' 
#' Online elastic net regression using the FTRL Proximal algorithm for training.
#'
#' This method is intended for matrix input. 
#'
#' @param x the model matrix containing features
#' @param y the response variable
#' @param lambda1 L1 regularization term
#' @param lambda2 L2 regularization term
#' @param a learning rate parameter
#' @param b learning rate parameter controlling decay, defaults to 1.
#' @param num_epochs number of times we should traverse over the traiing set, defaults to 1.
#' @param save_loss is to save the loss function during training.
#' @param ... additional args
#' @return ftrlprox model object
#' @author Vilhelm von Ehrenheim
#'
#' @method ftrlprox default
#' @useDynLib FTRLProximal
#' @export
##------------------------------------------------------------------------------
ftrlprox.default <- function(x, y, lambda1, lambda2, a, b=1, num_epochs=1,
                             save_loss=F, ...) {
  if (!is.factor(y))
    stop("Dependent variable must be a factor")

  if (nlevels(y) != 2)
    stop("Dependent variable must be a factor with 2 levels")

  # Make factor into numeric 0 and 1
  ynum <- as.numeric(y) - 1  

  is_sparse <- FALSE
  ix <- jx <- NULL
  if (inherits(x,"sparseMatrix")) {
    is_sparse <- TRUE
    x <- as(x,"CsparseMatrix")
    x <- as(x,"dgCMatrix")
  }

  J = if (save_loss) numeric(nrow(x)*num_epochs) else numeric(0)

  out <- if(is_sparse) {
          .C("splognet_ftrlprox",
             X=as.double(x@x),
             ix=as.integer(x@p),
             jx=as.integer(x@i),
             theta=double(ncol(x)),
             y=as.double(ynum),
             m=as.integer(nrow(x)),
             n=as.integer(ncol(x)),
             z=double(ncol(x)),
             nn=double(ncol(x)),
             J=J,
             num_epochs=as.integer(num_epochs),
             a=as.double(a),
             b=as.double(b),
             lambda1=as.double(lambda1),
             lambda2=as.double(lambda2),
             loss=as.integer(save_loss))
  } else {
          .C("lognet_ftrlprox",
             X=as.double(x),
             theta=double(ncol(x)),
             y=as.double(ynum),
             m=as.integer(nrow(x)),
             n=as.integer(ncol(x)),
             z=double(ncol(x)),
             nn=double(ncol(x)),
             J=J,
             num_epochs=as.integer(num_epochs),
             a=as.double(a),
             b=as.double(b),
             lambda1=as.double(lambda1),
             lambda2=as.double(lambda2),
             loss=as.integer(save_loss))
  }

  # Remove dataset from output
  out$X <- NULL
  out$y <- NULL

  if (is_sparse) {
      out$ix <- NULL
      out$jx <- NULL
  }

  # Set the feature colnames as parameter names
  names(out$theta) <- colnames(x)

  # Save target levels
  out$levels <- levels(y)

  class(out) <- "ftrlprox"
  out
}

