##------------------------------------------------------------------------------
#' Predict function for FTRLProx models
#' 
#' Predict outcome or probability of outcome using a regression model trained
#' using the FTRL PRoximal algorithm.
#' 
#' @param obj The model object to use for prediction.
#' @param newdata the new dataset to predict the outcome of.
#' @param type the type of response.
#' @return an array containing the predictions
#' 
#' @author Vilhelm von  Ehrenheim
#' @export
##------------------------------------------------------------------------------
predict.ftrlprox <- function(obj, newdata=NULL, type="response") {
  dep_var <- all.vars(obj$fx[[2]])
  newdata[[dep_var]] <- 0

  X <- model.matrix(obj$fx, newdata)

  out <- .C("lognet_predict",
            X=as.double(X),
            theta=as.double(obj$theta),
            y=numeric(nrow(X)),
            m=as.integer(nrow(X)),
            n=as.integer(ncol(X)))

  return(out$y)
}

