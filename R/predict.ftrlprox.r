##------------------------------------------------------------------------------
#' Predict function for FTRLProx models
#' 
#' Predict outcome or probability of outcome using a regression model trained
#' using the FTRL PRoximal algorithm.
#' 
#' @param object The model object to use for prediction.
#' @param newdata the new dataset to predict the outcome of.
#' @param type the type of response.
#' @param ... additional args
#' @return an array containing the predictions
#' 
#' @author Vilhelm von  Ehrenheim
#' @useDynLib FTRLProximal
#' @export
##------------------------------------------------------------------------------
predict.ftrlprox <- function(object, newdata=NULL, type="response", ...) {
  out <- .C("lognet_predict",
            X=as.double(newdata),
            theta=as.double(object$theta),
            y=numeric(nrow(newdata)),
            m=as.integer(nrow(newdata)),
            n=as.integer(ncol(newdata)))

  return(out$y)
}

