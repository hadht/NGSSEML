#' Predict ngssm.mle.p object.
#'
#' @param x ngssm.mle.p object
#' @param ... ignored
#'@noRd
summary.ngssm.mle<- function(object,...){
   objecta <-object$summary
   class(objecta) = "ngssm.mle"
  return(objecta)
}
#predict.ngssm.mle(fit,data=NULL)
#predict(fit,data=NULL)