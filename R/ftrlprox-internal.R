#' @noRd
#' @keywords internal
#'
#' Validate that `y` is a 2-level factor. If `levels` is supplied, also check
#' that the levels match exactly (used when extending a fitted model with
#' \code{update}).
.validate_response <- function(y, levels = NULL) {
  if (!is.factor(y))
    stop("Dependent variable must be a factor")

  if (nlevels(y) != 2)
    stop("Dependent variable must be a factor with 2 levels")

  if (!is.null(levels) && !isTRUE(all.equal(levels(y), levels)))
    stop("Dependent variable must have the same levels as original training data")

  invisible(y)
}


#' @noRd
#' @keywords internal
#'
#' Coerce sparse input to dgCMatrix so the sparse C kernel can read it directly.
#' Dense input is returned unchanged.
.coerce_features <- function(x) {
  if (inherits(x, "sparseMatrix")) {
    x <- as(x, "CsparseMatrix")
    x <- as(x, "dgCMatrix")
  }
  x
}


#' @noRd
#' @keywords internal
#'
#' Run the FTRL-Proximal C kernel for `num_epochs` passes over `x`/`ynum`,
#' starting from `(theta, z, nn)`. Dispatches to the sparse or dense kernel
#' based on the storage type of `x` (which must already have been passed
#' through \code{.coerce_features}). Returns the raw .C output list so callers
#' can do their own field cleanup.
.fit_step <- function(x, ynum, theta, z, nn,
                      lambda, alpha, a, b,
                      num_epochs, save_loss) {
  m <- nrow(x)
  n <- ncol(x)
  J <- if (save_loss) numeric(m * num_epochs) else numeric(0)

  lambda1 <- as.double(alpha * lambda)
  lambda2 <- as.double((1 - alpha) * lambda)

  if (inherits(x, "dgCMatrix")) {
    .C("splognet_ftrlprox",
       X = as.double(x@x),
       ix = as.integer(x@p),
       jx = as.integer(x@i),
       theta = as.double(theta),
       y = as.double(ynum),
       m = as.integer(m),
       n = as.integer(n),
       z = as.double(z),
       nn = as.double(nn),
       J = J,
       num_epochs = as.integer(num_epochs),
       a = as.double(a),
       b = as.double(b),
       lambda1 = lambda1,
       lambda2 = lambda2,
       loss = as.integer(save_loss))
  } else {
    .C("lognet_ftrlprox",
       X = as.double(x),
       theta = as.double(theta),
       y = as.double(ynum),
       m = as.integer(m),
       n = as.integer(n),
       z = as.double(z),
       nn = as.double(nn),
       J = J,
       num_epochs = as.integer(num_epochs),
       a = as.double(a),
       b = as.double(b),
       lambda1 = lambda1,
       lambda2 = lambda2,
       loss = as.integer(save_loss))
  }
}
