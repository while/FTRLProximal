##------------------------------------------------------------------------------
#' Predict function for FTRLProx models
#' 
#' Predict outcome or probability of outcome using a regression model trained
#' using the FTRL PRoximal algorithm.
#' 
#' @param object The model object to use for prediction.
#' @param newdata the new dataset to predict the outcome of.
#' @param type the type of response. Can be 'class' for class perdictions or
#' 'response' for probabilities. Default option is 'response'.
#' @param ... additional args
#' @return an array containing the predictions
#' 
#' @author Vilhelm von  Ehrenheim
#' @useDynLib FTRLProximal
#' @export
##------------------------------------------------------------------------------
predict.ftrlprox <- function(object, newdata=NULL, type=c("response", "class"), ...) {
  type=match.arg(type)

  out <- .C("lognet_predict",
            X=as.double(newdata),
            theta=as.double(object$theta),
            y=numeric(nrow(newdata)),
            m=as.integer(nrow(newdata)),
            n=as.integer(ncol(newdata)))

  switch(type,
         response = out$y,
         class = factor(ifelse(out$y<0.5, 1, 2), labels=object$levels))
}

