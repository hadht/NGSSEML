#Prediction(obj$formula,obj$data,na.action=obj$na.action,pz=obj$pz,nBreaks=obj$nBreaks,
#           model=obj$model,StaPar=obj$coefficients[1],a0=obj$a0,b0=obj$b0,distl="PRED",
#          ci=obj$ci,samples=1000,hh=hh,Xtprev=data,method="MLE")
#####Colocar essas funcoes ocultas!!!
##################################################################################
#My Predict.function: SmoothingF/PredictionF function # So Poisson e Gama Previsao
#'@noRd
ngssm.bayes.p<- function(object,data=NULL,hh=NULL){
  if(!is.null(hh)){
    if (object$model!= "Poisson" && object$model!="SRGamma" && object$model!="SRWeibull")stop("Sorry! Forecasting is not available for the model.")
  }
  if(is.null(hh)) {
    objecta <- object$fitted.values
  } else if (object$model=="Poisson" || object$model=="SRGamma" || object$model=="SRWeibull"){
    objecta<-Prediction(object$formula,object$data,na.action=object$na.action,pz=object$pz,nBreaks=object$nBreaks,
                       model=object$model,StaPar=object$samplepost,a0=object$a0,b0=object$b0,distl="PRED",
                       ci=object$ci,samples=1000,hh=hh,Xtprev=data,method="Bayes")
  }
  else{
    objecta <- object$fitted.values  
  }
  class(objecta) = "ngssm.bayes"
  return(objecta)
}


