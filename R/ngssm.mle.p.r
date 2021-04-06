#Prediction(obj$formula,obj$data,na.action=obj$na.action,pz=obj$pz,nBreaks=obj$nBreaks,
#           model=obj$model,StaPar=obj$coefficients[1],a0=obj$a0,b0=obj$b0,distl="PRED",
#          ci=obj$ci,samples=1000,hh=hh,Xtprev=data,method="MLE")
#####Colocar essas funcoes ocultas!!!
##################################################################################
#My Predict.function: SmoothingF/PredictionF function # So Poisson e Gama Previsao
#'@noRd
ngssm.mle.p<- function(object,data=NULL,hh=NULL){
  if(!is.null(hh)){
    if (object$model!= "Poisson" && object$model!="SRGamma" && object$model!="SRWeibull")stop("Sorry! Forecasting is not available for the model.")
  }
  if(is.null(hh) ) {
    objecta <- list(object$fitted.values)
  } else if (object$model=="Poisson" || object$model=="SRGamma" || object$model=="SRWeibull"){
    objecta<-list(Prediction(formula=object$formula,data=object$data,na.action=object$na.action,pz=object$pz,nBreaks=object$nBreaks,
                        model=object$model,StaPar=object$coefficients,a0=object$a0,b0=object$b0,distl="PRED",
                        ci=object$ci,samples=1000,hh=hh,Xtprev=data,method="MLE"))
  }
  else{
    objecta <- list(object$fitted.values)
  }
  class(objecta) = "ngssm.mle"
  return(objecta)
}
#predict.ngssm.mle(fit,data=NULL)
#predict(fit,data=NULL)