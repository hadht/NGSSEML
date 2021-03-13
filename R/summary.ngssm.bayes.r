#' Predict ngssm.mle.p object.
#'
#' @param x ngssm.mle.p object
#' @param ... ignored
#'@noRd
summary.ngssm.bayes<- function(object,...){
  if(is.matrix(object)){
    objecta <-object
  }else{
    objecta <-object$summary 
  }
   class(objecta) = "ngssm.bayes"
  return(objecta)
}
#predict.ngssm.mle(fit,data=NULL)
#predict(fit,data=NULL)