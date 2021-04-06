##
##
#'@noRd
PriorF<-function(StaPar,model="Poisson",prw=c(1,1),prnu=NULL,prchi=NULL,prmu=NULL,prbetamu=NULL,
prbetasigma=NULL){ # specify this function out as an input!

if (model=="Poisson" || model=="Gamma" || model=="GGamma" || model=="Weibull"||model=="SRGamma" || model=="SRWeibull" || model=="PEM"){
if(is.null(prmu)==FALSE)stop("Warning: Bad input for prmu!")
}
if (model=="Poisson" || model=="Gamma" || model=="Weibull"||model=="SRGamma" || model=="SRWeibull" || model=="PEM"){
if(is.null(prchi)==FALSE)stop("Warning: Bad input for prchi, instead of prnu!")
}
if (model=="Poisson" || model=="PEM" || model=="Normal" || model=="Laplace"){
if(is.null(prnu)==FALSE)stop("Warning: Bad input for prnu!")
}
if (model=="Gamma" || model=="Weibull"|| model=="GED"){
if(is.null(prchi)==FALSE)stop("Warning: prnu should be no null, instead of prchi!")
}

if (model!="GGamma"){
if(is.null(prnu)==FALSE && is.null(prchi)==FALSE)stop("Warning: Bad input for prnu or prchi!")
}  
 
if (model=="Poisson" || model=="Normal" || model=="Laplace" || model=="GED"||   #TS
     model=="Gamma" || model=="GGamma" || model=="Weibull"){
### PRIOR
pnu=dbeta(StaPar[1],prw[1],prw[2])  # Poisson/Normal/Laplace
if(is.null(prmu)==FALSE){ #Normal/Laplace  with mean
pnu=dbeta(StaPar[1],prw[1],prw[2])*dnorm(StaPar[2],prmu[1],prmu[2])
}
if(is.null(prbetamu)==FALSE){ # Poisson/Normal with covariates
if (is.null(prnu) && is.null(prchi)){ 
if(is.null(prmu)){ #Normal/Laplace with mean and covariates
pnu=dbeta(StaPar[1],prw[1],prw[2])*dmvnorm(StaPar[-(1)],prbetamu,prbetasigma)
}else{pnu=dbeta(StaPar[1],prw[1],prw[2])*dnorm(StaPar[2],prmu[1],prmu[2])*dmvnorm(StaPar[-(1:2)],prbetamu,prbetasigma)
}
}
}
################################################################################
if(is.null(prnu)==FALSE){ # Gamma/Weibull/GED  
if(is.null(prchi)){ 
pnu=dbeta(StaPar[1],prw[1],prw[2])*dgamma(StaPar[2],prnu[1],prnu[2])
if(is.null(prmu)==FALSE){ #GED with mean
pnu=dbeta(StaPar[1],prw[1],prw[2])*dgamma(StaPar[2],prnu[1],prnu[2])*dnorm(StaPar[3],prmu[1],prmu[2])
}
 if(is.null(prbetamu)==FALSE && is.null(prmu)==FALSE){

 if(is.null(prmu)){
 pnu=dbeta(StaPar[1],prw[1],prw[2])*dgamma(StaPar[2],prnu[1],prnu[2])*dmvnorm(StaPar[-(1:2)],prbetamu,prbetasigma)  # Gamma/Weibull/GED with covariates
 }
 if(is.null(prbetamu)==FALSE && is.null(prmu)==FALSE){ #GED with mean and covariates
  pnu=dbeta(StaPar[1],prw[1],prw[2])*dgamma(StaPar[2],prnu[1],prnu[2])*dnorm(StaPar[3],prmu[1],prmu[2])*dmvnorm(StaPar[-(1:3)],prbetamu,prbetasigma)
 }
 }
 }else{  # GGamma
 # cat("test!")
 pnu=dbeta(StaPar[1],prw[1],prw[2])*dgamma(StaPar[2],prnu[1],prnu[2])*dgamma(StaPar[3],prchi[1],prchi[2])
 if(is.null(prbetamu)==FALSE){pnu=dbeta(StaPar[1],prw[1],prw[2])*dgamma(StaPar[2],prnu[1],prnu[2])*dgamma(StaPar[3],prchi[1],prchi[2])*dmvnorm(StaPar[-(1:3)],prbetamu,prbetasigma)}    # GGamma with Covariate
}
}
################################################################################
}
if(model=="SRGamma" || model=="SRWeibull"){                #SR
### PRIOR
pnu=dbeta(StaPar[1],prw[1],prw[2])*dgamma(StaPar[2],prnu[1],prnu[2])
if(is.null(prbetamu)==FALSE){
pnu=dbeta(StaPar[1],prw[1],prw[2])*dgamma(StaPar[2],prnu[1],prnu[2])*dmvnorm(StaPar[-(1:2)],prbetamu,prbetasigma)
}
}
################################################################################
if(model=="PEM"){                                   #PEM
### PRIOR
p=length(StaPar)
pnu=dbeta(StaPar[1],prw[1],prw[2])
if(is.null(prbetamu)==FALSE){pnu=dbeta(StaPar[1],prw[1],prw[2])*dmvnorm(StaPar[-1],prbetamu,prbetasigma)}
}
################################################################################
#Final return:
return(pnu)
}
