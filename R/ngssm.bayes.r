################################################################################
##
##
## NGSSM: BAYESIAN ESTIMATION
##
##
################################################################################
# Global variables: 
#globalVariables("akima")
globalVariables("sd")
globalVariables("hist")
globalVariables("nsamplex")
globalVariables("da1")
#globalVariables("Break")
#globalVariables("txtProgressBar")
#'@export
ngssm.bayes<-function(formula, data,
                       na.action="na.omit",pz=NULL,nBreaks=NULL,model="Poisson",
                       StaPar=NULL,amp=FALSE,a0=0.01,b0=0.01,prw=c(1,1),prnu=NULL,prchi=NULL,prmu=NULL,prbetamu=NULL,
prbetasigma=NULL,lower=NULL,upper=NULL,ci=0.95,
pointss=10,nsamplex=1000,mcmc=NULL,postplot=FALSE,contourplot=FALSE,LabelParTheta=NULL,verbose=FALSE){
#Begin ngssm.bayes


#Dataframe data
#Dataframe data
#if(length(all.vars(formula))> dim(data)[2])stop("Check the formula and data.")
#if(is.data.frame(data)==FALSE)stop("The argument needs to be a data frame.")
call <- match.call()
oldoptions <-options(warn=-1)
on.exit(options(oldoptions)) 

Event=NULL
Break=NULL
#NA
 if(na.action=="na.omit"){na.omit(data)}

##Check formula
# if(check.env){
#    envs<-lapply(formula, environment)
#    hasenv<-which(sapply(envs,is.null))
#    if (length(hasenv)>1){
 #     for(i in 2:length(hasenv))
  #      if (!identical(envs[[hasenv[1]]],envs[[hasenv[i]]]))
#          warning("Different environments on formulas")
#    }
#  }

###################################################################################
###################################################################################
###################################################################################
nameaux=all.vars(formula)
nameaux=nameaux[-1]
namesz=paste("Z",1:2000,sep="")
xz<- nameaux %in% namesz
if(is.null(pz)){
  if(length(which(xz==TRUE))!=0)stop("Bad input for Z and pz!")
}else{
  if(pz!=length(which(xz==TRUE)))stop("Bad input for Z and pz!!")
}
# Event: PEM
if(model=="PEM"){
  namey=all.vars(formula)[1] #Y
  Y=get(namey,data)   # Y
  xze<- nameaux %in% "Event"
  if(length(which(xze==TRUE))==0)stop("Include an Event variable!")
  #all.vars(formula) =="Event"
  #rr=which(all.vars(formula) =="Event")
  #aa=all.vars(formula)[rr]
  Event=get("Event",data)
#  Break=GridP(Y, Event, nT = nBreaks)
  da2=model.frame(formula,data)
  namey=all.vars(formula)[1] #Y
  nameaux1y=all.vars(formula)
  xz1y<- nameaux1y %in% namey
  namexz1y=nameaux1y[xz1y==FALSE]
  da2=da2[namexz1y]
  Z=NULL
  if(dim(da2)[2]==1){X=NULL
  }else{
    nameaux1=all.vars(formula)
    nameaux1=nameaux1[-1]
    xz1<- nameaux1 %in% "Event"
    namexz1=nameaux1[xz1==FALSE]
    if(length(namexz1)==0){X=NULL}else{X=da2[namexz1]}
  }
  Break=GridP(Y, Event, nT = nBreaks)
}else{
  Event=NULL
  # End Event
  namey=all.vars(formula)[1] #Y
  Y=get(namey,data)   # Y
  #Y
  #names(data)=c("Y","X1","Z1","Z2","Z3")
  #fz <- Y~X1+Z1+Z2+Z3
  if(is.null(pz)){
    da2=model.frame(formula,data)
    namey=all.vars(formula)[1] #Y
    nameaux1y=all.vars(formula)
    xz1y<- nameaux1y %in% namey
    namexz1y=nameaux1y[xz1y==FALSE]
    if(dim(da2[namexz1y])[2]==0){X=NULL}else{da2=da2[namexz1y];X=da2}
    Z=NULL
  }else{
    #if(pz!=NULL){
    #names(da1)=c("Y","X1","Z1","Z2","Z3")
    #fz <- Y~X1+Z1+Z2+Z3
    namesz=paste("Z",1:pz,sep="")
    da2=model.frame(formula,da1)
    Z=da2[namesz]
    #Z
    nameaux=all.vars(formula)
    nameaux=nameaux[-1]
    xz<- nameaux %in% namesz
    namexz=nameaux[xz==FALSE]
    if(dim(da2[namexz])[2]==0){X=NULL}else{X=da2[namexz]}
    #X
    #}
  }
}
Yt<-Y
Xt<-X
Zt<-Z
#cat("Yt=",Yt)
#print(Xt)
#print(Zt)
#print(Event)
###################################################################################
###################################################################################
###################################################################################



# DataFrame:  
#dataf<-data  
#dataf<-dataf[all.vars(formula)]
##Dataframe data
#if(length(all.vars(formula))> dim(data)[2])stop("Check the formula and data.")
#if(is.data.frame(data)==FALSE)stop("The argument needs to be a data frame.")

#attach(dataf)
#Yt=get(names(dataf)[1])
#Ytdd=dataf[[colnames(dataf)[1]]]

#if(model=="PEM"){
#dataf<-data  
#dataf<-dataf[c(all.vars(formula)[1],colnames(data)[2],all.vars(formula)[-1])]
##Dataframe data
#if(length(all.vars(formula))> dim(data)[2])stop("Check the formula and data.")
#if(is.data.frame(data)==FALSE)stop("The argument needs to be a data frame.")

##dataf<-dataf[all.vars(formula)]
##Yt=get(names(dataf)[1])
#Ytdd=dataf[[colnames(dataf)[1]]]
##Event=get(names(dataf)[2])
#Eventdd=dataf[[colnames(dataf)[2]]]
#Breakdd=GridP(Ytdd, Eventdd, nT = nBreaks)
#Event<-Eventdd
#Break<-Breakdd
#Xtdd=NULL
#Ztdd=NULL
#if(is.null(pz)){
#if(dim(dataf)[2]>2){
#nnnd=dim(dataf)[1]
#ppd=dim(dataf)[2]-2
#Xtdd=matrix(0,nnnd,ppd)
#for(i in 1:ppd){
##Xt[,i]=get(names(dataf)[i+2])
#Xtdd[,i]=dataf[[names(dataf)[i+2]]] 

#}
#}
#}
# if(is.null(pz)!=TRUE){
#nnnd=dim(dataf)[1]
#ppd=dim(dataf)[2]-2-pz
#if(ppd>=1){
#Xtdd=matrix(0,nnnd,ppd)
#for(i in 1:ppd){
##Xt[,i]=get(names(dataf)[i+2])
#Xtdd[,i]=dataf[[names(dataf)[i+2]]]  
#}
#}
#if(pz>=1){
#Ztdd=matrix(0,nnnd,pz)
#for(j in 1:pz){
##Zt[,j]=get(names(dataf)[j+ppd+2])
#Ztdd[,j]=dataf[[names(dataf)[j+ppd+2]]]
#}
#}
#}
#}

#if(model!="PEM"){
#dataf<-data  
#dataf<-dataf[all.vars(formula)]
#Event<-NULL
#Break<-NULL

##Dataframe data
#if(length(all.vars(formula))> dim(data)[2])stop("Check the formula and data.")
#if(is.data.frame(data)==FALSE)stop("The argument needs to be a data frame.")
#Ytdd=dataf[[colnames(dataf)[1]]]

#Xtdd=NULL
#Ztdd=NULL
#if(is.null(pz)){
#if(dim(dataf)[2]>1){
#nnnd=dim(dataf)[1]
#ppd=dim(dataf)[2]-1
#Xtdd=matrix(0,nnnd,ppd)
#for(i in 1:ppd){
##Xt[,i]=get(names(dataf)[i+1])
###print(get(names(dataf)[i+1]))
#Xtdd[,i]=dataf[[names(dataf)[i+1]]]  

#}
#}
#}
#if(is.null(pz)!=TRUE){
#nnnd=dim(dataf)[1]
#ppd=dim(dataf)[2]-1-pz
#if(ppd>=1){
#Xtdd=matrix(0,nnnd,ppd)
#for(i in 1:ppd){
##Xt[,i]=get(names(dataf)[i+1])
#Xtdd[,i]=dataf[[names(dataf)[i+1]]]  
#}
#}
#if(pz>=1){
#Ztdd=matrix(0,nnnd,pz)
#for(j in 1:pz){
##Zt[,j]=get(names(dataf)[j+ppd+1])
#Ztdd[,j]=dataf[[names(dataf)[j+ppd+1]]]
#}
#}
#}
#}
#Yt<-Ytdd
#Xt<-Xtdd
#Zt<-Ztdd
##detach(dataf)
##print(Yt)
##print(Xt)
##print(Zt)
################################################################################


aans=FALSE
parindicator=StaPar
pa=1
if(is.null(prnu)==FALSE){pa=pa+1}
if(is.null(prchi)==FALSE){pa=pa+1}
if(is.null(prbetamu)==FALSE){pa=pa+length(prbetamu)}
if(is.null(prmu)==FALSE){pa=pa+1}

if(is.null(lower)==FALSE&&is.null(upper)==FALSE){ # lower and upper limits restiction!
if(is.null(StaPar)&&is.null(lower)==FALSE)stop("Specify a initial point to StarPar!")
if(is.null(StaPar)&&is.null(upper)==FALSE)stop("Specify a initial point to StarPar!")
pp=length(StaPar)
if(sum(as.integer(StaPar>lower))!=pp)stop("Specify an initial point inside the parametric space, the lower and upper limits!")
if(sum(as.integer(StaPar<upper))!=pp)stop("Specify an initial point inside the parametric space, the lower and upper limits!")
}
#if (is.null(LabelParTheta)==TRUE && is.null(StaPar)==TRUE)stop("Bad input for LabelParTheta for this model")
if (is.null(LabelParTheta)==FALSE && is.null(StaPar)==FALSE){
if(length(LabelParTheta)!=length(StaPar))stop("Bad input for StaPar and LabelParTheta for this model")
}
if(is.null(StaPar)){pp=length(LabelParTheta)}else{pp=length(StaPar)}
if(is.null(Xt)==FALSE){if(is.matrix(Xt)==FALSE){Xt=as.matrix(Xt)}}
if(is.null(Zt)==FALSE){if(is.matrix(Zt)==FALSE){Zt=as.matrix(Xt)}}
if(is.null(Xt)==FALSE){if(dim(Xt)[2]>18)stop("Many covariates!!!")}
if(is.null(Zt)==FALSE){if(dim(Zt)[2]>18)stop("Many covariates!!!")}

#if(pointss>100){warning("too many points!")}
#if(pointss<pp)stop("A few points!")
if(pointss<4)stop("A few points!")
if((pointss^pa)>20000){warning("Too many points to evaluate!")}
#if((pointss^pa)>500000)stop("Too many points to evaluate! It's impossible allocated memory!")
#if((pointss^pp)>20000){aans=ask.user.yn.question("Exact calculation may take a long time. Would you like to switch MCMC for a faster result?")}
if((pointss^pa)>50000){aans=TRUE}
if(is.null(mcmc)==FALSE){
if(mcmc==TRUE){aans=TRUE}
if(mcmc==FALSE){aans=FALSE}
}
if(aans){warning("Exact calculation may take a long time, so ARMS method is used!")}
#if((pointss^length(StaPar))>60000){aans=yesno2("Too many points to evaluate! Do you like to use MCMC?")}
if(aans){ #Begin ARMS
################################################################################
####   
#### Bayesian Inference/ARMS
####
################################################################################
#Begin ARMS
### Begin TS Models
if(nsamplex>10000){warning("Too many samples!")}
burnin=1000
nsamplex=nsamplex+burnin
if (model=="Poisson" || model=="Normal" || model=="Laplace" || model=="GED"||   #TS
     model=="Gamma" || model=="GGamma" || model=="Weibull"){
if (is.null(LabelParTheta)==FALSE && is.null(StaPar)==FALSE){
if(length(LabelParTheta)!=length(StaPar))stop("Bad input for LabelParTheta for this model")
}

if (model=="Poisson" || model=="Gamma" || model=="GGamma" || model=="Weibull"){
if(is.null(Zt)==FALSE)stop("Bad input for Zt for this model")
}
#if (is.null(Event)==FALSE)stop("Bad input Event for this model")
#if (is.null(Break)==FALSE)stop("Bad input Break for this model")
if (amp==TRUE)stop("Bad input for amp for this model")

#############################################################################################
if(is.null(LabelParTheta)){   #BeginDefaultLabel
if (model=="Poisson"){ #Begin Poisson
    if(is.null(Xt)){
          LabelParTheta=c("w")
    }else{
          pp=dim(Xt)[2]
          LabelParThetaaux=c("w","Beta1","Beta2","Beta3","Beta4","Beta5","Beta6","Beta7","Beta8","Beta9","Beta10",
          "Beta11","Beta12","Beta13","Beta14","Beta15","Beta16","Beta17","Beta18","Beta19","Beta20")
          LabelParTheta=LabelParThetaaux[1:(1+pp)]
          }

}
 if (model=="Normal"){  #Begin Normal
    if(is.null(Xt) && is.null(Zt)){
          LabelParTheta=c("w")
    }
    
    if(is.null(Xt)==TRUE && is.null(Zt)==FALSE){
          pp=dim(Xt)[2]
          LabelParThetaaux=c("w","Delta1","Delta2","Delta3","Delta4","Delta5","Delta6","Delta7","Delta8","Delta9","Delta10",
          "Delta11","Delta12","Delta13","Delta14","Delta15","Delta16","Delta17","Delta18","Delta19","Delta20")
          LabelParTheta=LabelParThetaaux[1:(1+pp)]
    }
    
    if(is.null(Xt)==FALSE && is.null(Zt)==TRUE){
          ppp=dim(Zt)[2]
          LabelParThetaaux=c("w","Beta1","Beta2","Beta3","Beta4","Beta5","Beta6","Beta7","Beta8","Beta9","Beta10",
          "Beta11","Beta12","Beta13","Beta14","Beta15","Beta16","Beta17","Beta18","Beta19","Beta20")
          LabelParTheta=LabelParThetaaux[1:(1+ppp)]
    }
    if(is.null(Xt)==FALSE && is.null(Zt)==FALSE){
          pp=dim(Xt)[2]
          ppp=dim(Zt)[2]
          LabelBeta=c("Beta1","Beta2","Beta3","Beta4","Beta5","Beta6","Beta7","Beta8","Beta9","Beta10",
          "Beta11","Beta12","Beta13","Beta14","Beta15","Beta16","Beta17","Beta18","Beta19","Beta20")
           LabelDelta=c("Delta1","Delta2","Delta3","Delta4","Delta5","Delta6","Delta7","Delta8","Delta9","Delta10",
          "Delta11","Delta12","Delta13","Delta14","Delta15","Delta16","Delta17","Delta18","Delta19","Delta20")
          LabelParTheta=numeric(1+pp+ppp)
          LabelParTheta[1]=c("w")
          LabelParTheta[2:(pp+1)]=LabelBeta[1:(pp)]
          LabelParTheta[(pp+2):(pp+1+ppp)]=LabelDelta[1:(ppp)]
    }
     
 }#EndNormal
 if (model=="Laplace"){  #Begin Laplace
    if(is.null(Xt) && is.null(Zt)){
          LabelParTheta=c("w")
    }
    
    if(is.null(Xt)==TRUE && is.null(Zt)==FALSE){
          pp=dim(Xt)[2]
          LabelParThetaaux=c("w","Delta1","Delta2","Delta3","Delta4","Delta5","Delta6","Delta7","Delta8","Delta9","Delta10",
          "Delta11","Delta12","Delta13","Delta14","Delta15","Delta16","Delta17","Delta18","Delta19","Delta20")
          LabelParTheta=LabelParThetaaux[1:(1+pp)]
    }
    
    if(is.null(Xt)==FALSE && is.null(Zt)==TRUE){
          ppp=dim(Zt)[2]
          LabelParThetaaux=c("w","Beta1","Beta2","Beta3","Beta4","Beta5","Beta6","Beta7","Beta8","Beta9","Beta10",
          "Beta11","Beta12","Beta13","Beta14","Beta15","Beta16","Beta17","Beta18","Beta19","Beta20")
          LabelParTheta=LabelParThetaaux[1:(1+ppp)]
    }
    if(is.null(Xt)==FALSE && is.null(Zt)==FALSE){
          pp=dim(Xt)[2]
          ppp=dim(Zt)[2]
          LabelBeta=c("Beta1","Beta2","Beta3","Beta4","Beta5","Beta6","Beta7","Beta8","Beta9","Beta10",
          "Beta11","Beta12","Beta13","Beta14","Beta15","Beta16","Beta17","Beta18","Beta19","Beta20")
           LabelDelta=c("Delta1","Delta2","Delta3","Delta4","Delta5","Delta6","Delta7","Delta8","Delta9","Delta10",
          "Delta11","Delta12","Delta13","Delta14","Delta15","Delta16","Delta17","Delta18","Delta19","Delta20")
          LabelParTheta=numeric(1+pp+ppp)
          LabelParTheta[1]=c("w")
          LabelParTheta[2:(pp+1)]=LabelBeta[1:(pp)]
          LabelParTheta[(pp+2):(pp+1+ppp)]=LabelDelta[1:(ppp)]
    }
 } #EndLaplace
 if (model=="GED"){   #Begin GED
      
    if(is.null(Xt) && is.null(Zt)){
          LabelParTheta=c("w","nu")
    }
    
    if(is.null(Xt)==TRUE && is.null(Zt)==FALSE){
          ppp=dim(Zt)[2]
           LabelParThetaaux=c("w","nu","Delta1","Delta2","Delta3","Delta4","Delta5","Delta6","Delta7","Delta8","Delta9","Delta10",
          "Delta11","Delta12","Delta13","Delta14","Delta15","Delta16","Delta17","Delta18","Delta19","Delta20")
          LabelParTheta=LabelParThetaaux[1:(2+ppp)] 
    }
    
    if(is.null(Xt)==FALSE && is.null(Zt)==TRUE){
          pp=dim(Xt)[2]
           LabelParThetaaux=c("w","nu","Beta1","Beta2","Beta3","Beta4","Beta5","Beta6","Beta7","Beta8","Beta9","Beta10",
          "Beta11","Beta12","Beta13","Beta14","Beta15","Beta16","Beta17","Beta18","Beta19","Beta20")     
          LabelParTheta=LabelParThetaaux[1:(2+pp)]
         
    }
    if(is.null(Xt)==FALSE && is.null(Zt)==FALSE){
          pp=dim(Xt)[2]
          ppp=dim(Zt)[2]
          LabelBeta=c("Beta1","Beta2","Beta3","Beta4","Beta5","Beta6","Beta7","Beta8","Beta9","Beta10",
          "Beta11","Beta12","Beta13","Beta14","Beta15","Beta16","Beta17","Beta18","Beta19","Beta20")
           LabelDelta=c("Delta1","Delta2","Delta3","Delta4","Delta5","Delta6","Delta7","Delta8","Delta9","Delta10",
          "Delta11","Delta12","Delta13","Delta14","Delta15","Delta16","Delta17","Delta18","Delta19","Delta20")
          LabelParTheta=numeric(1+pp+ppp)
          LabelParTheta[1:2]=c("w","nu")
          LabelParTheta[3:(pp+2)]=LabelBeta[1:(pp)]
          LabelParTheta[(pp+3):(pp+2+ppp)]=LabelDelta[1:(ppp)]
    }
     
 }#EndGED
 if (model=="Gamma"){     #Begin Gamma
        if(is.null(Xt)){
          LabelParTheta=c("w","nu")
    }else{
          pp=dim(Xt)[2]
          LabelParThetaaux=c("w","nu","Beta1","Beta2","Beta3","Beta4","Beta5","Beta6","Beta7","Beta8","Beta9","Beta10",
          "Beta11","Beta12","Beta13","Beta14","Beta15","Beta16","Beta17","Beta18","Beta19","Beta20")
          LabelParTheta=LabelParThetaaux[1:(2+pp)]
          }
 }
 if (model=="GGamma"){     #Begin GGamma
           if(is.null(Xt)){
          LabelParTheta=c("w","nu","chi")
    }else{
          pp=dim(Xt)[2]
          LabelParThetaaux=c("w","nu","chi","Beta1","Beta2","Beta3","Beta4","Beta5","Beta6","Beta7","Beta8","Beta9","Beta10",
          "Beta11","Beta12","Beta13","Beta14","Beta15","Beta16","Beta17","Beta18","Beta19","Beta20")
          LabelParTheta=LabelParThetaaux[1:(3+pp)]
          }
 }
  if (model=="Weibull"){   #Begin GED/Gamma/Weibull
           if(is.null(Xt)){
          LabelParTheta=c("w","nu")
    }else{
          pp=dim(Xt)[2]
          LabelParThetaaux=c("w","nu","Beta1","Beta2","Beta3","Beta4","Beta5","Beta6","Beta7","Beta8","Beta9","Beta10",
          "Beta11","Beta12","Beta13","Beta14","Beta15","Beta16","Beta17","Beta18","Beta19","Beta20")
          LabelParTheta=LabelParThetaaux[1:(2+pp)]
          }
 }

}#EndDefaultLabel
#############################################################################################

if(verbose) cat ("\n*****Non-Gaussian State Space Models with Exact Likelihood*****\n",
"\nNGSSMEL Package:","Bayes -",model,"\n")
set.seed(1000)

#############################################################################################
##Start values StaPar
if ((is.null(LabelParTheta)==FALSE)){
 if(is.null(StaPar)==FALSE){
    if(length(LabelParTheta)!=length(StaPar))stop("Bad input for LabelParTheta for this model")
 }
}
#print("TEST=============================")

if (model=="Poisson"){ #Begin Poisson
##StaPar:

if(is.null(Xt)){dxt=0}else{dxt=dim(Xt)[2];}
if(is.null(Zt)){dzt=0}else{dzt=0;}
p=(1+dxt+0);
if(is.null(StaPar)==FALSE){
StaPar1=StaPar;
StaPar1[1]=log(-log(StaPar[1]));
};
if(is.null(StaPar)){ #Begin StaPar
StaPar=numeric(p);StaPar1=StaPar;
StaPar[1]=0.8;StaPar1[1]=log(-log(StaPar[1]));if(dxt>0){StaPar[2:(dxt+1)]=rep(0,dxt);StaPar1[2:(dxt+1)]=StaPar[2:(dxt+1)];};
}#EndStaPar
##lower:
#if(is.null(lower)){
#lower=numeric(p);
#blbeta=-10;
#bubeta=10;
#lower[1]=0.01;if(dxt>0){lower[2:(dxt+1)]=rep(blbeta,dxt);};
#};
##upper:
#if(is.null(upper)){
#upper=numeric(p);
#blmu=-10;
#bumu=10;
#upper[1]=0.999;if(dxt>0){upper[2:(dxt+1)]=rep(bumu,dxt);};
#};
} #End Poisson

if (model=="Normal"){  #Begin Normal
##StaPar:
if(is.null(Xt)){dxt=0}else{dxt=dim(Xt)[2];}
if(is.null(Zt)){dzt=0}else{dzt=dim(Zt)[2];}
p=(1+dxt+dzt);
if(is.null(StaPar)==FALSE){
StaPar1=StaPar;
StaPar1[1]=log(-log(StaPar[1]));
};
if(is.null(StaPar)){ #Begin StaPar
StaPar=numeric(p);StaPar1=StaPar;
StaPar[1]=0.8;StaPar1[1]=log(-log(StaPar[1]));if(dxt>0){StaPar[2:(dxt+1)]=rep(0,dxt);};if(dzt>0){StaPar[(dxt+1):(dxt+1+dzt)]=rep(0,dzt);StaPar1[(dxt+1):(dxt+1+dzt)]=StaPar[(dxt+1):(dxt+1+dzt)];};
}#EndStaPar

##lower:
#if(is.null(lower)){
#lower=numeric(p);
#blbeta=-100;
#bubeta=100;
#lower[1]=0.001;if(dxt>0){lower[2:(dxt+1)]=rep(blbeta,dxt);};if(dzt>0){lower[(dxt+1):(dxt+1+dzt)]=rep(blbeta,dzt);};
#};
##upper:
#if(is.null(upper)){
#upper=numeric(p);
#blmu=-100;
#bumu=100;
#upper[1]=0.999;if(dxt>0){upper[2:(dxt+1)]=rep(bumu,dxt);};if(dzt>0){upper[(dxt+1):(dxt+1+dzt)]=rep(bumu,dzt);};
#};
}#End Normal

if (model=="Laplace"){  #Begin Laplace
##StaPar:
if(is.null(Xt)){dxt=0}else{dxt=dim(Xt)[2];}
if(is.null(Zt)){dzt=0}else{dzt=dim(Zt)[2];}
p=(1+dxt+dzt);
if(is.null(StaPar)==FALSE){
StaPar1=StaPar;
StaPar1[1]=log(-log(StaPar[1]));
};
if(is.null(StaPar)){ #Begin StaPar
StaPar=numeric(p);StaPar1=StaPar;
StaPar[1]=0.8;StaPar1[1]=log(-log(StaPar[1]));if(dxt>0){StaPar[2:(dxt+1)]=rep(0,dxt);};if(dzt>0){StaPar[(dxt+1):(dxt+1+dzt)]=rep(0,dzt);StaPar1[(dxt+1):(dxt+1+dzt)]=StaPar[(dxt+1):(dxt+1+dzt)];};
}#EndStaPar

##lower:
#if(is.null(lower)){
#lower=numeric(p);
#blbeta=-100;
#bubeta=100;
#lower[1]=0.001;if(dxt>0){lower[2:(dxt+1)]=rep(blbeta,dxt);};if(dzt>0){lower[(dxt+1):(dxt+1+dzt)]=rep(blbeta,dzt);};
#};
##upper:
#if(is.null(upper)){
#upper=numeric(p);
#blmu=-100;
#bumu=100;
#upper[1]=0.999;if(dxt>0){upper[2:(dxt+1)]=rep(bumu,dxt);};if(dzt>0){upper[(dxt+1):(dxt+1+dzt)]=rep(bumu,dzt);};
#};
} #End Laplace

if (model=="GED"){   #Begin GED
##StaPar:
if(is.null(Xt)){dxt=0}else{dxt=dim(Xt)[2];}
if(is.null(Zt)){dzt=0}else{dzt=dim(Zt)[2];}
p=(1+1+dxt+dzt);
if(is.null(StaPar)==FALSE){
StaPar1=StaPar;
StaPar1[1]=log(-log(StaPar[1]));StaPar1[2]=log(StaPar[2]);
};
if(is.null(StaPar)){ #Begin StaPar
StaPar=numeric(p); StaPar1=StaPar;
StaPar[1]=0.9;StaPar1[1]=log(-log(StaPar[1]));StaPar[2]=1;StaPar1[2]=log(StaPar[2]);
}#EndStaPar

if(dxt>0){StaPar[3:(dxt+2)]=rep(0,dxt);StaPar1[3:(dxt+2)]=StaPar[3:(dxt+2)];};if(dzt>0){StaPar[(dxt+3):(dxt+2+dzt)]=rep(0,dzt);StaPar1[(dxt+3):(dxt+2+dzt)]=StaPar[(dxt+3):(dxt+2+dzt)];};
##lower:
#if(is.null(lower)){
#lower=numeric(p);
#blbeta=-10;
#bubeta=10;
#lower[1]=0.01;lower[2]=0.01;if(dxt>0){lower[3:(dxt+2)]=rep(blbeta,dxt);};if(dzt>0){lower[(dxt+3):(dxt+2+dzt)]=rep(blbeta,dzt);};
#print(lower)
#};
##upper:
#if(is.null(upper)){
#upper=numeric(p);
#blmu=-10;
#bumu=10;
#upper[1]=0.99;upper[2]=100;if(dxt>0){upper[3:(dxt+2)]=rep(bumu,dxt);};if(dzt>0){upper[(dxt+3):(dxt+2+dzt)]=rep(bumu,dzt);};
##print(upper)
#};

}#End GED

if (model=="Gamma"){     #Begin Gamma
##StaPar:
if(is.null(Xt)){dxt=0}else{dxt=dim(Xt)[2];}
if(is.null(Zt)){dzt=0}else{dzt=dim(Zt)[2];}
p=(1+1+dxt+dzt);
if(is.null(StaPar)==FALSE){
StaPar1=StaPar;
StaPar1[1]=log(-log(StaPar[1]));StaPar1[2]=log(StaPar[2]);
};
if(is.null(StaPar)){ #Begin StaPar
StaPar=numeric(p);StaPar1=StaPar;
StaPar[1]=0.8;StaPar1[1]=log(-log(StaPar[1]));StaPar[2]=1;StaPar1[2]=log(StaPar[2]);if(p>2){StaPar[3:(dxt+2)]=rep(0,dxt);StaPar1[3:(dxt+2)]=StaPar[3:(dxt+2)];};
}#EndStaPar

##lower:
#if(is.null(lower)){
#lower=numeric(p);
#blbeta=-1000;
#bubeta=1000;
#lower[1]=0.001;lower[2]=0.001;if(p>2){lower[3:(dxt+2)]=rep(blbeta,dxt);};
#};
##upper:
#if(is.null(upper)){
#upper=numeric(p);
#blmu=-1000;
#bumu=1000;
#upper[1]=0.999;upper[2]=1000;if(p>2){upper[3:(dxt+2)]=rep(bumu,dxt);};
#};

}#End Gamma

if (model=="GGamma"){     #Begin GGamma
##StaPar:
if(is.null(Xt)){dxt=0}else{dxt=dim(Xt)[2];}
if(is.null(Zt)){dzt=0}else{dzt=dim(Zt)[2];}
p=(1+1+dxt+dzt);
if(is.null(StaPar)==FALSE){
StaPar1=StaPar;
StaPar1[1]=log(-log(StaPar[1]));StaPar1[2]=log(StaPar[2]);StaPar1[3]=log(StaPar[3]);
};
if(is.null(StaPar)){ #Begin StaPar
StaPar=numeric(p);StaPar1=StaPar;
StaPar[1]=0.8;StaPar1[1]=log(-log(StaPar[1]));StaPar[2]=1;StaPar1[2]=log(StaPar[2]);StaPar[3]=1;StaPar1[3]=log(StaPar[3]);if(p>3){StaPar[4:(dxt+2)]=rep(0,dxt);StaPar1[4:(dxt+2)]=StaPar[4:(dxt+2)];};
}#EndStaPar

##lower:
#if(is.null(lower)){
#lower=numeric(p);
#blbeta=-1000;
#bubeta=1000;
#lower[1]=0.001;lower[2]=0.001;lower[3]=0.001;if(p>3){lower[4:(dxt+2)]=rep(blbeta,dxt);};
#};
##upper:
#if(is.null(upper)){
#upper=numeric(p);
#blmu=-1000;
#bumu=1000;
#upper[1]=0.999;upper[2]=1000;upper[3]=1000;if(p>3){upper[4:(dxt+2)]=rep(bumu,dxt);};
#};
}#End GGamma

if (model=="Weibull"){    #Begin Weibull
##StaPar:
if(is.null(Xt)){dxt=0}else{dxt=dim(Xt)[2];}
if(is.null(Zt)){dzt=0}else{dzt=dim(Zt)[2];}
p=(1+dxt+dzt);
if(is.null(StaPar)==FALSE){
StaPar1=StaPar;
StaPar1[1]=log(-log(StaPar[1]));StaPar[2]=1;StaPar1[2]=log(StaPar[2]);
};
if(is.null(StaPar)){ #Begin StaPar
StaPar=numeric(p);StaPar1=StaPar;
StaPar[1]=0.8;StaPar1[1]=log(-log(StaPar[1]));StaPar[2]=1;StaPar1[2]=log(StaPar[2]);if(p>2){StaPar[3:(dxt+2)]=rep(0,dxt);StaPar1[3:(dxt+2)]=StaPar[3:(dxt+2)];};
}#EndStaPar

##lower:
#if(is.null(lower)){
#lower=numeric(p);
#blbeta=-1000;
#bubeta=1000;
#lower[1]=0.001;lower[2]=0.001;if(p>2){lower[3:(dxt+2)]=rep(blbeta,dxt);};
#};
##upper:
#if(is.null(upper)){
#upper=numeric(p);
#blmu=-1000;
#bumu=1000;
#upper[1]=0.999;upper[2]=1000;if(p>2){upper[3:(dxt+2)]=rep(bumu,dxt);};
#};

} #End Weibull

#}#End Default StarPar
#############################################################################################
# DEVERIA COLOCAR DEFAULT VALOR INICIAL E LIMITES LOWER AND UPPER!
#require(dlm)
# If default of StaPar is true, initialize StaPar.  
#if(is.null(StaPar)==FALSE){
#StaPar1=StaPar;
##Begin Default StarPar
#StaPar1[1]=log(-log(StaPar[1]));
#if (model=="GED" | model=="Gamma" | model=="Weibull"){   #Begin GED/Gamma/Weibull 
#StaPar1[2]=log(StaPar[2]);
#};
#if (model=="GGamma"){   #Begin GGamma 
#StaPar1[2]=log(StaPar[2]);StaPar1[3]=log(StaPar[3]);
#};
#};

#############################
if(is.null(upper)&&is.null(lower)){   # end limits

#############################
###GRID 
#############################

nn=length(Yt)
resultsopt=NA
resultsopt=tryCatch(optim(StaPar1, hessian=TRUE,LikeF2,
#lower=lower,upper=upper,
method="BFGS",na.action=na.action,Yt=Yt,Xt=Xt,Zt=Zt,Break=Break,Event=Event,
a0=a0,b0=b0,model=model,control = list(maxit = 30000, temp = 2000,
trace = FALSE,REPORT = 500)), error = c)
if(resultsopt$convergence!=0)stop("Convergence error! Bad inputs! Sorry!")
estopt=resultsopt$par     # Point Estimates:
estopt
Hessianmatrixopt=-resultsopt$hessian   #Hessian Matrix
Hessianmatrixopt
MIFopt=-solve(Hessianmatrixopt)       #MIF
MIFopt
MIFopt[1,1]=MIFopt[1,1]*(((exp(-exp(estopt[1])))*(-exp(estopt[1])))^2)
MIFopt
estopt[1]=exp(-exp(estopt[1]))
LI=estopt-1*(qnorm((1+ci)/2))*sqrt(diag(MIFopt))
LS=estopt+1*(qnorm((1+ci)/2))*sqrt(diag(MIFopt))

if (model=="Gamma"){
MIFopt[2,2]=MIFopt[2,2]*(((exp(estopt[2])))^2)
MIFopt
estopt[2]=exp((estopt[2]))
LI=estopt-1*(qnorm((1+ci)/2))*sqrt(diag(MIFopt))
LS=estopt+1*(qnorm((1+ci)/2))*sqrt(diag(MIFopt))
#LI[1]=exp(-exp(estopt[1]+1*(qnorm((1+ci)/2))*sqrt((MIFopt[1,1]))))
#LS[1]=exp(-exp(estopt[1]-1*(qnorm((1+ci)/2))*sqrt((MIFopt[1,1]))))
if(LI[2]<0){LI[2]=0}
}
if (model=="Weibull"){
MIFopt[2,2]=MIFopt[2,2]*(((exp(estopt[2])))^2)
MIFopt
estopt[2]=exp((estopt[2]))
LI=estopt-1*(qnorm((1+ci)/2))*sqrt(diag(MIFopt))
LS=estopt+1*(qnorm((1+ci)/2))*sqrt(diag(MIFopt))
#LI[1]=exp(-exp(estopt[1]+1*(qnorm((1+ci)/2))*sqrt((MIFopt[1,1]))))
#LS[1]=exp(-exp(estopt[1]-1*(qnorm((1+ci)/2))*sqrt((MIFopt[1,1]))))
if(LI[2]<0){LI[2]=0}
}
if (model=="GED"){
MIFopt[2,2]=MIFopt[2,2]*(((exp(estopt[2])))^2)
MIFopt
estopt[2]=exp((estopt[2]))
LI=estopt-1*(qnorm((1+ci)/2))*sqrt(diag(MIFopt))
LS=estopt+1*(qnorm((1+ci)/2))*sqrt(diag(MIFopt))
#LI[1]=exp(-exp(estopt[1]+1*(qnorm((1+ci)/2))*sqrt((MIFopt[1,1]))))
#LS[1]=exp(-exp(estopt[1]-1*(qnorm((1+ci)/2))*sqrt((MIFopt[1,1]))))
#if(LI[2]<0){LI[2]=0}
}
if (model=="GGamma"){
MIFopt[2,2]=MIFopt[2,2]*(((exp(estopt[2])))^2)
MIFopt[3,3]=MIFopt[3,3]*(((exp(estopt[3])))^2)
MIFopt
estopt[2:3]=exp((estopt[2:3]))
LI=estopt-1*(qnorm((1+ci)/2))*sqrt(diag(MIFopt))
LS=estopt+1*(qnorm((1+ci)/2))*sqrt(diag(MIFopt))
#LI[1]=exp(-exp(estopt[1]+1*(qnorm((1+ci)/2))*sqrt((MIFopt[1,1]))))
#LS[1]=exp(-exp(estopt[1]-1*(qnorm((1+ci)/2))*sqrt((MIFopt[1,1]))))
#if(LI[2]<0){LI[2]=0}
#if(LI[3]<0){LI[3]=0}
}

qn=5
p=length(StaPar1)
linf=numeric(p)
lsup=numeric(p)
#linf[1]=estopt[1]-6*sqrt(MIFopt[1,1])
#lsup[1]=estopt[1]+6*sqrt(MIFopt[1,1])
for(jj in 1:p){   # Intervals for Grid
linf[jj]=estopt[jj]-qn*sqrt(MIFopt[jj,jj])
lsup[jj]=estopt[jj]+qn*sqrt(MIFopt[jj,jj])
}
if(linf[1]<0){linf[1]=1e-20}
#if(linf[1]>0.9){linf[1]=0.85}
if(lsup[1]>1){lsup[1]=0.99999}
if(lsup[1]==1){lsup[1]=0.99999}
StaPar=estopt;
lower=linf
upper=lsup
#############################
}   # end limits
#############################

#StaPar[1]=estopt[1]-0.01;
#if(StaPar[1]<0){StaPar[1]=estopt[1]+0.02};
#if(StaPar[1]>1){StaPar[1]=estopt[1]-0.02};
#StaPar1[1]=log(-log(StaPar[1]));StaPar1[2]=log(StaPar[2]);


Log.Dens<-function(StaPar,formula=formula, data=data,na.action=na.action,pz=pz,nBreaks=nBreaks,model,a0,b0,lower,upper,
prw,prnu,prchi,prmu,prbetamu,prbetasigma){ 
log.post=-LikeF(StaPar,Yt=Yt,Xt=Xt,Zt=Zt,Break=Break,Event=Event,na.action=na.action,
model=model,a0=a0,b0=b0)+log(PriorF(StaPar,
model=model,prw=prw,prnu=prnu,prchi=prchi,prmu=prmu,prbetamu=prbetamu,prbetasigma=prbetasigma))
return(log.post)
 }
Log.Dens.Suport<-function(StaPar,formula=formula,data=data,na.action=na.action,pz=pz,nBreaks=nBreaks,model,a0,b0,lower,upper,
prw,prnu,prchi,prmu,prbetamu,prbetasigma){
p=length(StaPar)
prodd=numeric(p)
for(i in 1:p) {
  prodd[i]=as.numeric((StaPar[i]>lower[i])*(StaPar[i]<upper[i]))
}
return(prod(prodd))
 } 
message("\nTime:.")
message("\nPosterior inputs...")
message("\nDone!")
message("\nTime:..")
message("\nPosterior computations...")
message("\nWait...") 
message("\nARMS Running...") 
if(is.null(parindicator)){StaPar=estopt}
nuout<- arms(y.start = StaPar, myldens = Log.Dens, 
indFunc = Log.Dens.Suport, n.sample = nsamplex,lower=lower,upper=upper,
formula=formula, data=data,na.action=na.action,pz=pz,nBreaks=nBreaks,
model=model,a0=a0,b0=b0,prw=prw,prnu=prnu,prchi=prchi,prmu=prmu,
prbetamu=prbetamu,prbetasigma=prbetasigma) 
#print(dim(nuout))
message("\nDone!")
p=length(StaPar)
burnin=1000
nuout=as.matrix(nuout)
#if(is.vector(nuout)){nuout<-nuout[(burnin+1):nsamplex]}
#else{
nuout<-nuout[(burnin+1):nsamplex,1:p]
nuout=as.matrix(nuout)
#}
#print(nuout)
#print(colnames(nuout))
colnames(nuout)=LabelParTheta
#if(is.na(warnings()[1])==FALSE){warning("Try to change the lower and upper arguments of the MCMC procedure! Only a warning message!")}
if(is.null(dim(nuout))){nuout=t(t(nuout))}
colnames(nuout)=LabelParTheta
q11=apply(nuout,2,quantile,probs=(1-ci)/2)
q21=apply(nuout,2,quantile,probs=0.5)
q31=apply(nuout,2,quantile,probs=ci+(1-ci)/2)
meanest=apply(nuout,2,mean)
sdest=apply(nuout,2,sd)
p=length(StaPar)
mfit<-matrix(c(meanest,q21,sdest,q11,q31),p,5)
colnames(mfit)=c("Mean","Median","Sd","Lower","Upper")
rownames(mfit)=paste0(c('\u03b8'),1:p)
if(is.null(LabelParTheta)==FALSE){rownames(mfit)=LabelParTheta}
nn=length(Yt)
ngssm.list<-list(mfit,ci*100,nn)
names(ngssm.list)<-c("Bayesian Estimation","Nom. Level(%)","n.obs")
#names(ngssm.list)<-c("Mean","Sd","Lower","Median","Upper","Nom. Level(%)","n.obs")
#cat("\n*****Non-Gaussian State Space Models with Exact Likelihood*****\n",
#"\nNGSSMEL Package:","Bayes -",model,"\n")
if(verbose) print (ngssm.list)
ngssm.list<-list(mfit,ci*100,nn,meanest,formula)
names(ngssm.list)<-c("Bayesian Estimation","Nom. Level(%)","n.obs","coefficients","formula")

#Graphs:
if(p>1){
if(contourplot==TRUE){
scatterplotMatrix(nuout,var.labels=LabelParTheta)
}
}
if(postplot==TRUE){
 #dev.new()
oldpar <- par(mar=rep(2, 4),mfrow=c(p,2))
on.exit(par(oldpar)) #line i+1
#par(mar=rep(2, 4))
#par(mfrow=c(p,2))
for(ii in 1:p){
hist(nuout[,ii],xlab="",main=LabelParTheta[ii])
lines(c(mean(nuout[,ii]),mean(nuout[,ii])),c(0,nsamplex),lwd=2,col="red") # posterior mean
lines(c(quantile(nuout[,ii],probs=((1-ci)/2)),quantile(nuout[,ii],probs=((1-ci)/2))),c(0,nsamplex/10),lwd=2,col="blue",lty=2) # posterior mean
lines(c(quantile(nuout[,ii],probs=(ci+(1-ci)/2)),quantile(nuout[,ii],probs=(ci+(1-ci)/2))),c(0,nsamplex/10),lwd=2,col="blue",lty=2) # posterior mean
legend("topleft", c("Posterior Mean","Cred.Int.95%"),col=c("red","blue"),lty=c(1,2),bty = "n",cex=0.5)
}
} # end contour plot

#if(is.na(warnings()[1])==FALSE){warning("Try to change the lower and upper arguments!")}
#cat("\nTime:....")
#cat("\nGraphs...")
#cat("\nDone!")
postsample=nuout
colnames(postsample)=LabelParTheta
ngssm.list=list(ngssm.list,postsample)
message("End!")
#return(ngssm.list)
}#End TS Models 
################################################################################
################################################################################


################################################################################
################################################################################
if(model=="SRGamma" || model=="SRWeibull"){                #SR
#if(model=="SRGamma"){model1="Gamma"}
#if(model=="SRWeibull"){model1="Weibull"}
#if (is.null(Event)==FALSE)stop("Bad input Event for this model")
#if (is.null(Break)==FALSE)stop("Bad input Break for this model")
if (amp==TRUE)stop("Bad input for amp for this model")
if(is.null(Zt)==FALSE)stop("Bad input for Zt for this model")

#############################################################################################
if(is.null(LabelParTheta)){   #BeginDefaultLabel
if (model=="SRGamma"){ #Begin Poisson
    if(is.null(Xt)){
          LabelParTheta=c("w","nu")
    }else{
          pp=dim(Xt)[2]
          LabelParThetaaux=c("w","nu","Beta1","Beta2","Beta3","Beta4","Beta5","Beta6","Beta7","Beta8","Beta9","Beta10",
          "Beta11","Beta12","Beta13","Beta14","Beta15","Beta16","Beta17","Beta18","Beta19","Beta20")
          LabelParTheta=LabelParThetaaux[1:(2+pp)]
          }

}
if (model=="SRWeibull"){ #Begin Poisson
    if(is.null(Xt)){
          LabelParTheta=c("w","nu")
    }else{
          pp=dim(Xt)[2]
          LabelParThetaaux=c("w","nu","Beta1","Beta2","Beta3","Beta4","Beta5","Beta6","Beta7","Beta8","Beta9","Beta10",
          "Beta11","Beta12","Beta13","Beta14","Beta15","Beta16","Beta17","Beta18","Beta19","Beta20")
          LabelParTheta=LabelParThetaaux[1:(2+pp)]
          }

}
}#EndDefaultLabel
#############################################################################################

################################################################################
##
## Weibull Example 
##
################################################################################
set.seed(1000)
#require(dlm)
# If default of StaPar is true, initialize StaPar.
#Begin Default StarPar
#if(is.null(StaPar)){
if (model=="SRGamma"){     #Begin Gamma
##StaPar:
if(is.null(Xt)){dxt=0}else{dxt=dim(Xt)[2];}
if(is.null(Zt)){dzt=0}else{dzt=dim(Zt)[2];}
p=(1+1+dxt+dzt);

if(is.null(StaPar)){ #Begin StaPar
StaPar=numeric(p);StaPar1=StaPar;
StaPar[1]=0.9;StaPar1[1]=log(-log(StaPar[1]));StaPar[2]=1;StaPar1[2]=log(StaPar[2]);if(p>2){StaPar[3:(dxt+2)]=rep(0.0,dxt);StaPar1[3:(dxt+2)]=StaPar[3:(dxt+2)];};
}#EndStaPar
#cat("Star=",StaPar)
#cat("Star1=",StaPar1)
#StaPar1=StaPar;
#StaPar1[1]=log(-log(StaPar[1]));StaPar1[2]=log(StaPar[2]);

##lower:
#if(is.null(lower)){
#lower=numeric(p);
#blbeta=-5;
#bubeta=5;
#lower[1]=0.001;lower[2]=0.001;if(p>2){lower[3:(dxt+2)]=rep(blbeta,dxt);};
#};
##upper:
#if(is.null(upper)){
#upper=numeric(p);
#blmu=-5;
#bumu=5;
#upper[1]=0.999;upper[2]=1000;if(p>2){upper[3:(dxt+2)]=rep(bumu,dxt);};
#};

}#End Gamma

if (model=="SRWeibull"){    #Begin Weibull
##StaPar:
#print(StaPar);
if(is.null(Xt)){dxt=0}else{dxt=dim(Xt)[2];}
if(is.null(Zt)){dzt=0}else{dzt=dim(Zt)[2];}
p=(1+1+dxt+dzt);
if(is.null(StaPar)){ #Begin StaPar
StaPar=numeric(p);StaPar1=StaPar;
#StaPar[1]=0.999;StaPar1[1]=log(-log(StaPar[1]));StaPar[2]=0.75;StaPar1[2]=log(StaPar[2]);if(p>2){StaPar[3:(dxt+2)]=rep(0.01,dxt);};
StaPar[1]=0.95;StaPar1[1]=log(-log(StaPar[1]));StaPar[2]=0.75;StaPar1[2]=log(StaPar[2]);if(p>2){StaPar[3:(dxt+2)]=rep(0,dxt);StaPar1[3:(dxt+2)]=StaPar[3:(dxt+2)];};
}#EndStaPar
#StaPar1=StaPar;
#StaPar1[1]=log(-log(StaPar[1]));StaPar1[2]=log(StaPar[2]);
#print(StaPar);
##lower:
#if(is.null(lower)){
#lower=numeric(p);
#blbeta=-2;
#bubeta=2;
#lower[1]=0.01;lower[2]=0.01;if(p>2){lower[3:(dxt+2)]=rep(blbeta,dxt);};
#};
##upper:
#if(is.null(upper)){
#upper=numeric(p);
#blmu=-2;
#bumu=2;
#upper[1]=0.99;upper[2]=3;if(p>2){upper[3:(dxt+2)]=rep(bumu,dxt);};
#};
#print(StaPar);
} #End Weibull

#}#End Default StarPar
#############################################################################################
# DEVERIA COLOCAR DEFAULT VALOR INICIAL E LIMITES LOWER AND UPPER!
if(is.null(StaPar)==FALSE){
StaPar1=StaPar;
StaPar1[1]=log(-log(StaPar[1]));
StaPar1[2]=log(StaPar[2]);
};

#############################
if(is.null(upper)&&is.null(lower)){   # end limits

#############################
###GRID
#############################
nn=length(Yt)
resultsopt=NA
resultsopt=tryCatch(optim(StaPar1, hessian=TRUE,LikeF2,
#lower=lower,upper=upper,
method="BFGS",na.action=na.action,Yt=Yt,Xt=Xt,Zt=Zt,Break=Break,Event=Event,
a0=a0,b0=b0,model=model,control = list(maxit = 30000, temp = 2000,
trace = FALSE,REPORT = 500)), error = c)
if(is.null(resultsopt$convergence)){stop("Convergence error! Bad inputs! Sorry!")}else{
if(resultsopt$convergence!=0)stop("Convergence error! Bad inputs! Sorry!")}
#if(is.null(warnings()[1])==FALSE){warning("Try to specify a better initial point for the static parameters.")}

estopt=resultsopt$par     # Point Estimates:
estopt
Hessianmatrixopt=-resultsopt$hessian   #Hessian Matrix
Hessianmatrixopt
MIFopt=-solve(Hessianmatrixopt)       #MIF
MIFopt

MIFopt[1,1]=MIFopt[1,1]*(((exp(-exp(estopt[1])))*(-exp(estopt[1])))^2)
MIFopt[2,2]=MIFopt[2,2]*(((exp(estopt[2])))^2)
MIFopt
estopt[1]=exp(-exp(estopt[1]))
estopt[2]=exp((estopt[2]))
LI=estopt-1*(qnorm((1+ci)/2))*sqrt(diag(MIFopt))
LS=estopt+1*(qnorm((1+ci)/2))*sqrt(diag(MIFopt))


qn=3
p=length(StaPar1)
linf=numeric(p)
lsup=numeric(p)
#linf[1]=estopt[1]-8*sqrt(MIFopt[1,1])
#lsup[1]=estopt[1]+8*sqrt(MIFopt[1,1])
for(jj in 1:p){   # Intervals for Grid
linf[jj]=estopt[jj]-qn*sqrt(MIFopt[jj,jj])
lsup[jj]=estopt[jj]+qn*sqrt(MIFopt[jj,jj])
}
lsup[1]=lsup[1]-0.0000001
if(linf[1]<0){linf[1]=1e-5}
if(linf[1]>0.9){linf[1]=0.85}
if(lsup[1]>1){lsup[1]=0.99999;estopt[1]=estopt[1]-0.001}
if(lsup[1]==1){lsup[1]=0.99999;estopt[1]=estopt[1]-0.001}
if(linf[2]<1e-5){linf[2]=1e-5}
lower=linf
upper=lsup
StaPar=estopt
#############################
}   # end limits
#############################
#if (is.null(Event)==FALSE)stop("Bad input Event for this model")
#StaPar=estopt;

#if(StaPar[1]<0){StaPar[1]=estopt[1]+0.02}
#if(StaPar[1]>1){StaPar[1]=estopt[1]-0.02}
#StaPar1[1]=log(-log(StaPar[1]));StaPar1[2]=log(StaPar[2]);

if(verbose) cat ("\n*****Non-Gaussian State Space Models with Exact Likelihood*****\n",
"\nNGSSEML Package:","Bayes -",model,"\n")
message("\nTime:.")
message("\nPosterior inputs...")
message("\nDone!")
message("\nTime:..")
message("\nPosterior computations...")

Log.Dens<-function(StaPar,formula=formula, data=data,na.action=na.action,pz=pz,nBreaks=nBreaks,
model,a0,b0,lower,upper,
prw,prnu,prchi,prmu,prbetamu,prbetasigma){ 
log.post=-LikeF(StaPar,na.action=na.action,Yt=Yt,Xt=Xt,Zt=Zt,Break=Break,Event=Event,
model=model,a0=a0,b0=b0)+log(PriorF(StaPar,
model=model,prw=prw,prnu=prnu,prchi=prchi,prmu=prmu,prbetamu=prbetamu,prbetasigma=prbetasigma))
return(log.post)
 }
Log.Dens.Suport<-function(StaPar,formula=formula, data=data,na.action=na.action,pz=pz,nBreaks=nBreaks,
model,a0,b0,lower,upper,prw,prnu,prchi,prmu,prbetamu,prbetasigma){
p=length(StaPar)
prodd=numeric(p)
for(i in 1:p) {
  prodd[i]=as.numeric((StaPar[i]>lower[i])*(StaPar[i]<upper[i]))
}
return(prod(prodd))
 }
#cat("\nPosterior computations...")
message("\nWait...") 
message("\nARMS Running...") 
#cat("\nStaPar...",StaPar) 
#cat("\nestopt...",estopt)
#cat("\nlower...",lower)
#cat("\nupper...",upper)
#if((StaPar[1]==0.999) && (StaPar[2]==0.75) && (StaPar[3]==0.01)){StaPar=estopt}
if(is.null(parindicator)){StaPar=estopt}
#cat("\nStaPar...",StaPar) 
#if((StaPar[1]==0.9) && (StaPar[2]==1) && (StaPar[3]==0)){StaPar=estopt}
nuout<- arms(y.start = StaPar, myldens = Log.Dens, 
indFunc = Log.Dens.Suport, n.sample =nsamplex,lower=lower,upper=upper,
formula=formula,data=data,na.action=na.action,pz=pz,nBreaks=nBreaks,
model=model,a0=a0,b0=b0,prw=prw,prnu=prnu,prchi=prchi,prmu=prmu,
prbetamu=prbetamu,prbetasigma=prbetasigma)
message("\nDone!\n")
p=length(StaPar)
burnin=1000
nuout=as.matrix(nuout)
#if(is.vector(nuout)){nuout<-nuout[(burnin+1):nsamplex]}
#else{
nuout<-nuout[(burnin+1):nsamplex,1:p]
nuout=as.matrix(nuout)
#}
#print(nuout)
#print(colnames(nuout))
colnames(nuout)=LabelParTheta
#if(is.na(warnings()[1])==FALSE){warning("Try to change the lower and upper arguments of the MCMC procedure! Only a warning message!")}
colnames(nuout)=LabelParTheta
q11=apply(nuout,2,quantile,probs=(1-ci)/2)
q21=apply(nuout,2,quantile,probs=0.5)
q31=apply(nuout,2,quantile,probs=ci+(1-ci)/2)
meanest=apply(nuout,2,mean)
sdest=apply(nuout,2,sd)
p=length(StaPar)
mfit<-matrix(c(meanest,q21,sdest,q11,q31),p,5)
colnames(mfit)=c("Mean","Median","Sd","Lower","Upper")
rownames(mfit)=paste0(c('\u03b8'),1:p)
if(is.null(LabelParTheta)==FALSE){rownames(mfit)=LabelParTheta}
nn=length(Yt)
ngssm.list<-list(mfit,ci*100,nn)
names(ngssm.list)<-c("Bayesian Estimation","Nom. Level(%)","n.obs")
#names(ngssm.list)<-c("Mean","Sd","Lower","Median","Upper","Nom. Level(%)","n.obs")
if(verbose) print (ngssm.list)
ngssm.list<-list(mfit,ci*100,nn,meanest,formula)
names(ngssm.list)<-c("Bayesian Estimation","Nom. Level(%)","n.obs","coefficients","formula")

#cat("\n*****Non-Gaussian State Space Models with Exact Likelihood*****\n",
#"\nNGSSMEL Package:","Bayes -",model,"\n")
#Graphs:
if(p>1){
if(contourplot==TRUE){
scatterplotMatrix(nuout,var.labels=LabelParTheta)
}
} # end contour plot
if(postplot==TRUE){
 #dev.new()
oldpar <- par(mar=rep(2, 4),mfrow=c(p,2))
on.exit(par(oldpar)) 
#par(mar=rep(2, 4))
#par(mfrow=c(p,2))
for(ii in 1:p){
hist(nuout[,ii],xlab="",main=LabelParTheta[ii])
lines(c(mean(nuout[,ii]),mean(nuout[,ii])),c(0,nsamplex),lwd=2,col="red") # posterior mean
lines(c(quantile(nuout[,ii],probs=((1-ci)/2)),quantile(nuout[,ii],probs=((1-ci)/2))),c(0,nsamplex/10),lwd=2,col="blue",lty=2) # posterior mean
lines(c(quantile(nuout[,ii],probs=(ci+(1-ci)/2)),quantile(nuout[,ii],probs=(ci+(1-ci)/2))),c(0,nsamplex/10),lwd=2,col="blue",lty=2) # posterior mean
legend("topleft", c("Posterior Mean","Cred.Int.95%"),col=c("red","blue"),lty=c(1,2),bty = "n",cex=0.5)
}
} # end post plot

postsample=nuout
colnames(postsample)=LabelParTheta
ngssm.list=list(ngssm.list,postsample)
message("End!")
#return(ngssm.list)
} #end SR Models
################################################################################
################################################################################


################################################################################
################################################################################
if(model=="PEM"){                                   #PEM
#amp=FALSE
if(is.null(Zt)==FALSE)stop("Bad input for Zt for this model")
################################################################################ 
#
## PEM Example 
##
################################################################################ 
#if(length(LabelParTheta)!=length(StaPar))stop("Bad input for LabelParTheta for this model")}
# If default of StaPar is true, initialize StaPar.
#Begin Default StarPar

#############################################################################################
if(is.null(LabelParTheta)){   #BeginDefaultLabel
if (model=="PEM"){ #Begin Poisson
    if(is.null(Xt)){
          LabelParTheta=c("w")
    }else{
          pp=dim(Xt)[2]
          LabelParThetaaux=c("w","Beta1","Beta2","Beta3","Beta4","Beta5","Beta6","Beta7","Beta8","Beta9","Beta10",
          "Beta11","Beta12","Beta13","Beta14","Beta15","Beta16","Beta17","Beta18","Beta19","Beta20")
          LabelParTheta=LabelParThetaaux[1:(1+pp)]
          }

}
}#EndDefaultLabel
#############################################################################################

#if(is.null(StaPar)){
if (model=="PEM"){ #Begin PEM
##StaPar:
if(is.null(Xt)){dxt=0}else{dxt=dim(Xt)[2];}
if(is.null(Zt)){dzt=0}else{dzt=0;}
pp=(1+dxt+0);
if(is.null(StaPar)){ #Begin StaPar
StaPar=numeric(pp);StaPar1=StaPar;
StaPar[1]=0.9;StaPar1[1]=log(-log(StaPar[1]));if(dxt>0){StaPar[2:(dxt+1)]=rep(0,dxt);StaPar1[2:(dxt+1)]=StaPar[2:(dxt+1)];};
}#EndStaPar
if(is.null(StaPar)==FALSE){
StaPar1=StaPar;
StaPar1[1]=log(-log(StaPar[1]));
};

##lower:
#if(is.null(lower)){
#lower=numeric(pp);
#blbeta=-100;
#bubeta=100;
#lower[1]=0.001;if(dxt>0){lower[2:(dxt+1)]=rep(blbeta,dxt);};
#};
##upper:
#if(is.null(upper)){
#upper=numeric(pp);
#blmu=-100;
#bumu=100;
#upper[1]=0.999;if(dxt>0){upper[2:(dxt+1)]=rep(bumu,dxt);};
#};
} #End PEM

#}#End Default StarPar
#print(StaPar)
#print(lower)
#print(upper)
#if(is.null(StaPar)==FALSE){
#StaPar1=StaPar;
#StaPar1[1]=log(-log(StaPar[1]));
#};
# DEVERIA COLOCAR DEFAULT VALOR INICIAL E LIMITES LOWER AND UPPER!
#############################################################################################
#############################
if(is.null(upper)&&is.null(lower)){   # end limits
nn=length(Yt)
resultsopt=NA
resultsopt=tryCatch(optim(StaPar1, hessian=TRUE,LikeF2,
#lower=lower,upper=upper,
method="BFGS",na.action=na.action,Yt=Yt,Xt=Xt,Zt=Zt,Break=Break,Event=Event,
amp=amp,a0=a0,b0=b0,model=model,control = list(maxit = 30000, temp = 2000,
trace = FALSE,REPORT = 500)), error = c)
if(is.null(resultsopt$convergence)){stop("Convergence error! Bad inputs! Sorry!")}else{
if(resultsopt$convergence!=0)stop("Convergence error! Bad inputs! Sorry!")}
estopt=resultsopt$par     # Point Estimates:
estopt
Hessianmatrixopt=-resultsopt$hessian   #Hessian Matrix
Hessianmatrixopt
MIFopt=-solve(Hessianmatrixopt)       #MIF

MIFopt[1,1]=MIFopt[1,1]*(((exp(-exp(estopt[1])))*(-exp(estopt[1])))^2)
MIFopt
estopt[1]=exp(-exp(estopt[1]))

qn=5
p=length(StaPar1)
linf=numeric(p)
lsup=numeric(p)
#linf[1]=estopt[1]-6*sqrt(MIFopt[1,1])
#lsup[1]=estopt[1]+6*sqrt(MIFopt[1,1])
for(jj in 1:p){   # Intervals for Grid
linf[jj]=estopt[jj]-qn*sqrt(MIFopt[jj,jj])
lsup[jj]=estopt[jj]+qn*sqrt(MIFopt[jj,jj])
}
lsup[1]=lsup[1]-0.0000001
if(linf[1]<0){linf[1]=1e-20}
if(linf[1]>0.9){linf[1]=0.85}
if(lsup[1]>1){lsup[1]=0.99999}
if(lsup[1]==1){lsup[1]=0.99999}

StaPar=estopt;
#
lower=linf
upper=lsup
#############################
}   # end limits
#############################

#StaPar[1]=estopt[1]-0.01;
#if(StaPar[1]<0){StaPar[1]=estopt[1]+0.02}
#if(StaPar[1]>1){StaPar[1]=estopt[1]-0.02}
#StaPar1[1]=log(-log(StaPar[1]));StaPar1[2]=log(StaPar[2]);


set.seed(1000)
if(verbose) cat ("\n*****Non-Gaussian State Space Models with Exact Likelihood*****\n",
"\nNGSSMEL Package:","Bayes -",model,"\n")
message("\nTime:.")
message("\nPosterior inputs...")
message("\nDone!")
message("\nTime:..")


Log.Dens<-function(StaPar,formula=formula, data=data,na.action=na.action,pz=pz,nBreaks=nBreaks,
model,a0,b0,lower,upper,prw,prnu,prchi,prmu,prbetamu,prbetasigma){ 
log.post=-LikeF(StaPar,na.action=na.action,Yt=Yt,Xt=Xt,Zt=Zt,Break=Break,Event=Event,
model=model,a0=a0,b0=b0)+log(PriorF(StaPar,
model=model,prw=prw,prnu=prnu,prchi=prchi,prmu=prmu,prbetamu=prbetamu,prbetasigma=prbetasigma))
return(log.post)
 }
Log.Dens.Suport<-function(StaPar,formula=formula, data=data,na.action=na.action,pz=pz,nBreaks=nBreaks,
model,a0,b0,lower,upper,prw,prnu,prchi,prmu,prbetamu,prbetasigma){
p=length(StaPar)
prodd=numeric(p)
for(i in 1:p) {
  prodd[i]=as.numeric((StaPar[i]>lower[i])*(StaPar[i]<upper[i]))
}
return(prod(prodd))
 } 
message("\nPosterior computations...")
message("\nWait...") 
message("\nARMS Running...") 
#cat("\nStaPar...",StaPar) 
#cat("\nestopt...",estopt)
#cat("\nlower...",lower)
#cat("\nupper...",upper)
if(is.null(parindicator)){StaPar=estopt} 
nuout<- arms(y.start = StaPar, myldens = Log.Dens, 
indFunc = Log.Dens.Suport, n.sample =nsamplex,lower=lower,upper=upper,
formula=formula, data=data,na.action=na.action,pz=pz,nBreaks=nBreaks,
model=model,a0=a0,b0=b0,prw=prw,prnu=prnu,prchi=prchi,prmu=prmu,
prbetamu=prbetamu,prbetasigma=prbetasigma)
message("\nDone!\n")
p=length(StaPar)
burnin=1000
nuout=as.matrix(nuout)
#if(is.vector(nuout)){nuout<-nuout[(burnin+1):nsamplex]}
#else{
nuout<-nuout[(burnin+1):nsamplex,1:p]
nuout=as.matrix(nuout)
#}
#print(nuout)
#print(colnames(nuout))
colnames(nuout)=LabelParTheta
#if(is.na(warnings()[1])==FALSE){warning("Try to change the lower and upper arguments of the MCMC procedure! Only a warning message!")}
#print(nuout)
message(LabelParTheta)
q11=apply(nuout,2,quantile,probs=(1-ci)/2)
q21=apply(nuout,2,quantile,probs=0.5)               
q31=apply(nuout,2,quantile,probs=ci+(1-ci)/2)
meanest=apply(nuout,2,mean)
sdest=apply(nuout,2,sd)
p=length(StaPar)
mfit<-matrix(c(meanest,q21,sdest,q11,q31),p,5)
colnames(mfit)=c("Mean","Median","Sd","Lower","Upper")
rownames(mfit)=paste0(c('\u03b8'),1:p)
if(is.null(LabelParTheta)==FALSE){rownames(mfit)=LabelParTheta}
nn=length(Yt)
ngssm.list<-list(mfit,ci*100,nn)
names(ngssm.list)<-c("Bayesian Estimation","Nom. Level(%)","n.obs")
#names(ngssm.list)<-c("Mean","Sd","Lower","Median","Upper","Nom. Level(%)","n.obs")
#cat("\n*****Non-Gaussian State Space Models with Exact Likelihood*****\n",
#"\nNGSSMEL Package:","Bayes -",model,"\n")
if(verbose) print (ngssm.list)
ngssm.list<-list(mfit,ci*100,nn,meanest,formula)
names(ngssm.list)<-c("Bayesian Estimation","Nom. Level(%)","n.obs","coefficients","formula")

#Graphs:
if(p>1){
if(contourplot==TRUE){
scatterplotMatrix(nuout,var.labels=LabelParTheta)
}
} # end contour plot
if(postplot==TRUE){
 #dev.new()
oldpar <- par(mar=rep(2, 4),mfrow=c(p,2))
on.exit(par(oldpar)) 
#par(mar=rep(2, 4))
#par(mfrow=c(p,2))
for(ii in 1:p){
hist(nuout[,ii],xlab="",main=LabelParTheta[ii])
lines(c(mean(nuout[,ii]),mean(nuout[,ii])),c(0,nsamplex),lwd=2,col="red") # posterior mean
lines(c(quantile(nuout[,ii],probs=((1-ci)/2)),quantile(nuout[,ii],probs=((1-ci)/2))),c(0,nsamplex/10),lwd=2,col="blue",lty=2) # posterior mean
lines(c(quantile(nuout[,ii],probs=(ci+(1-ci)/2)),quantile(nuout[,ii],probs=(ci+(1-ci)/2))),c(0,nsamplex/10),lwd=2,col="blue",lty=2) # posterior mean
legend("topleft", c("Posterior Mean","Cred.Int.95%"),col=c("red","blue"),lty=c(1,2),bty = "n",cex=0.5)
}
} # end post plot
postsample=nuout
colnames(postsample)=LabelParTheta
ngssm.list=list(ngssm.list,postsample)
message("End!")
#return(ngssm.list)
}#End PEM Model
################################################################################
################################################################################

#return(ngssm.list)
obj <- list()
fitfit<-list(model, formula, mfit[,1])
fit<-list(sys = sys.call(),"Mean.Post",mfit[,1])
names(fit)[3]="coefficients"
obj$fit <-fit
obj$model<-model
obj$formula<-formula
obj$pz<-pz
obj$a0<-a0
obj$b0<-b0
obj$nBreaks<-nBreaks
obj$ci<-ci
obj$na.action<-na.action
if(aans){method="ARMS"}else{method="Numerical Integration"}
obj$method<-list("Bayesian Estimation",method)
obj$nBreaks<-nBreaks
obj$pointss<-pointss
obj$nsample<-nsamplex
obj$cov <- cov(ngssm.list[[2]])
estoptc<-mfit[,1]
if(is.null(LabelParTheta)){names(estoptc)=paste0(c('\u03b8'),1:p)}else{names(estoptc)=LabelParTheta}
obj$coefficients<-list("Mean.Post",estoptc)
obj$data<-data
MeanSmooth=SmoothingF(formula=formula,data=data,model=model,
                      a0=a0,b0=b0,pz=pz,Type="Marg",amp=amp,samples=1,ci=ci,splot=FALSE,StaPar=ngssm.list[[2]])
aaff<-MeanSmooth[[1]][,1]
nnpp<-nn
if(model=="PEM"){nnpp<-length(Break)-1}
names(aaff)<-1:nnpp
obj$fitted.values<-list("Smoothed estimates",aaff) # FilteringF function
obj$y<-obj$fitted.values
#names(obj$y)<-c("Smoothed estimates")
pnn<-length(obj$fitted.values)
obj$x<-1:pnn
#names(obj$x)<-c("Order obs.")
#obj$summary<-list(cat ("\n*****Non-Gaussian State Space Models with Exact Likelihood*****\n","\nNGSSEML Package:","Bayes -",model,"\n"),ngssm.list[[1]]) #colocar a lista que criei de output
obj$summary<-list("*****Non-Gaussian State Space Models with Exact Likelihood*****\nNGSSEML Package:Bayes -",model,ngssm.list[[1]]) #colocar a lista que criei de output
obj$samplepost<-ngssm.list[[2]]
class(obj) = "ngssm.bayes"
ob<-ngssm.list[[1]]; 
class(ob) = "ngssm.bayes"
if(verbose==TRUE) {obj<-obj}else{obj<-ob}
return(obj) 

#End ARMS
################################################################################
################################################################################

}else{
################################################################################
################################################################################
# Begin Numerical integration
if((pointss^pp)>500000)stop("Too many points to evaluate! It's impossible allocated memory!")
if(nsamplex>10000){warning("Too many samples!")}
if (model=="Poisson" || model=="Normal" || model=="Laplace" || model=="GED"||   #TS
     model=="Gamma" || model=="GGamma" || model=="Weibull"){
if (is.null(Event)==FALSE)stop("Bad input Event for this model")
if (is.null(Break)==FALSE)stop("Bad input Break for this model")
if (amp==TRUE)stop("Bad input for amp for this model")
#if (is.null(LabelParTheta)==FALSE){
if (model=="Poisson" || model=="Gamma" || model=="GGamma" || model=="Weibull"){
if(is.null(Zt)==FALSE)stop("Bad input for Zt for this model")}

#############################################################################################
if(is.null(LabelParTheta)){   #BeginDefaultLabel
if (model=="Poisson"){ #Begin Poisson
    if(is.null(Xt)){
          LabelParTheta=c("w")
    }else{
          pp=dim(Xt)[2]
          LabelParThetaaux=c("w","Beta1","Beta2","Beta3","Beta4","Beta5","Beta6","Beta7","Beta8","Beta9","Beta10",
          "Beta11","Beta12","Beta13","Beta14","Beta15","Beta16","Beta17","Beta18","Beta19","Beta20")
          LabelParTheta=LabelParThetaaux[1:(1+pp)]
          }

}
 if (model=="Normal"){  #Begin Normal
    if(is.null(Xt) && is.null(Zt)){
          LabelParTheta=c("w")
    }
    
    if(is.null(Xt)==TRUE && is.null(Zt)==FALSE){
          pp=dim(Xt)[2]
          LabelParThetaaux=c("w","Delta1","Delta2","Delta3","Delta4","Delta5","Delta6","Delta7","Delta8","Delta9","Delta10",
          "Delta11","Delta12","Delta13","Delta14","Delta15","Delta16","Delta17","Delta18","Delta19","Delta20")
          LabelParTheta=LabelParThetaaux[1:(1+pp)]
    }
    
    if(is.null(Xt)==FALSE && is.null(Zt)==TRUE){
          ppp=dim(Zt)[2]
          LabelParThetaaux=c("w","Beta1","Beta2","Beta3","Beta4","Beta5","Beta6","Beta7","Beta8","Beta9","Beta10",
          "Beta11","Beta12","Beta13","Beta14","Beta15","Beta16","Beta17","Beta18","Beta19","Beta20")
          LabelParTheta=LabelParThetaaux[1:(1+ppp)]
    }
    if(is.null(Xt)==FALSE && is.null(Zt)==FALSE){
          pp=dim(Xt)[2]
          ppp=dim(Zt)[2]
          LabelBeta=c("Beta1","Beta2","Beta3","Beta4","Beta5","Beta6","Beta7","Beta8","Beta9","Beta10",
          "Beta11","Beta12","Beta13","Beta14","Beta15","Beta16","Beta17","Beta18","Beta19","Beta20")
           LabelDelta=c("Delta1","Delta2","Delta3","Delta4","Delta5","Delta6","Delta7","Delta8","Delta9","Delta10",
          "Delta11","Delta12","Delta13","Delta14","Delta15","Delta16","Delta17","Delta18","Delta19","Delta20")
          LabelParTheta=numeric(1+pp+ppp)
          LabelParTheta[1]=c("w")
          LabelParTheta[2:(pp+1)]=LabelBeta[1:(pp)]
          LabelParTheta[(pp+2):(pp+1+ppp)]=LabelDelta[1:(ppp)]
    }
     
 }#EndNormal
 if (model=="Laplace"){  #Begin Laplace
    if(is.null(Xt) && is.null(Zt)){
          LabelParTheta=c("w")
    }
    
    if(is.null(Xt)==TRUE && is.null(Zt)==FALSE){
          pp=dim(Xt)[2]
          LabelParThetaaux=c("w","Delta1","Delta2","Delta3","Delta4","Delta5","Delta6","Delta7","Delta8","Delta9","Delta10",
          "Delta11","Delta12","Delta13","Delta14","Delta15","Delta16","Delta17","Delta18","Delta19","Delta20")
          LabelParTheta=LabelParThetaaux[1:(1+pp)]
    }
    
    if(is.null(Xt)==FALSE && is.null(Zt)==TRUE){
          ppp=dim(Zt)[2]
          LabelParThetaaux=c("w","Beta1","Beta2","Beta3","Beta4","Beta5","Beta6","Beta7","Beta8","Beta9","Beta10",
          "Beta11","Beta12","Beta13","Beta14","Beta15","Beta16","Beta17","Beta18","Beta19","Beta20")
          LabelParTheta=LabelParThetaaux[1:(1+ppp)]
    }
    if(is.null(Xt)==FALSE && is.null(Zt)==FALSE){
          pp=dim(Xt)[2]
          ppp=dim(Zt)[2]
          LabelBeta=c("Beta1","Beta2","Beta3","Beta4","Beta5","Beta6","Beta7","Beta8","Beta9","Beta10",
          "Beta11","Beta12","Beta13","Beta14","Beta15","Beta16","Beta17","Beta18","Beta19","Beta20")
           LabelDelta=c("Delta1","Delta2","Delta3","Delta4","Delta5","Delta6","Delta7","Delta8","Delta9","Delta10",
          "Delta11","Delta12","Delta13","Delta14","Delta15","Delta16","Delta17","Delta18","Delta19","Delta20")
          LabelParTheta=numeric(1+pp+ppp)
          LabelParTheta[1]=c("w")
          LabelParTheta[2:(pp+1)]=LabelBeta[1:(pp)]
          LabelParTheta[(pp+2):(pp+1+ppp)]=LabelDelta[1:(ppp)]
    }
 } #EndLaplace
 if (model=="GED"){   #Begin GED
      
    if(is.null(Xt) && is.null(Zt)){
          LabelParTheta=c("w","nu")
    }
    
    if(is.null(Xt)==TRUE && is.null(Zt)==FALSE){
          ppp=dim(Zt)[2]
           LabelParThetaaux=c("w","nu","Delta1","Delta2","Delta3","Delta4","Delta5","Delta6","Delta7","Delta8","Delta9","Delta10",
          "Delta11","Delta12","Delta13","Delta14","Delta15","Delta16","Delta17","Delta18","Delta19","Delta20")
          LabelParTheta=LabelParThetaaux[1:(2+ppp)] 
    }
    
    if(is.null(Xt)==FALSE && is.null(Zt)==TRUE){
          pp=dim(Xt)[2]
           LabelParThetaaux=c("w","nu","Beta1","Beta2","Beta3","Beta4","Beta5","Beta6","Beta7","Beta8","Beta9","Beta10",
          "Beta11","Beta12","Beta13","Beta14","Beta15","Beta16","Beta17","Beta18","Beta19","Beta20")     
          LabelParTheta=LabelParThetaaux[1:(2+pp)]
         
    }
    if(is.null(Xt)==FALSE && is.null(Zt)==FALSE){
          pp=dim(Xt)[2]
          ppp=dim(Zt)[2]
          LabelBeta=c("Beta1","Beta2","Beta3","Beta4","Beta5","Beta6","Beta7","Beta8","Beta9","Beta10",
          "Beta11","Beta12","Beta13","Beta14","Beta15","Beta16","Beta17","Beta18","Beta19","Beta20")
           LabelDelta=c("Delta1","Delta2","Delta3","Delta4","Delta5","Delta6","Delta7","Delta8","Delta9","Delta10",
          "Delta11","Delta12","Delta13","Delta14","Delta15","Delta16","Delta17","Delta18","Delta19","Delta20")
          LabelParTheta=numeric(1+pp+ppp)
          LabelParTheta[1:2]=c("w","nu")
          LabelParTheta[3:(pp+2)]=LabelBeta[1:(pp)]
          LabelParTheta[(pp+3):(pp+2+ppp)]=LabelDelta[1:(ppp)]
    }
     
 }#EndGED
 if (model=="Gamma"){     #Begin Gamma
        if(is.null(Xt)){
          LabelParTheta=c("w","nu")
    }else{
          pp=dim(Xt)[2]
          LabelParThetaaux=c("w","nu","Beta1","Beta2","Beta3","Beta4","Beta5","Beta6","Beta7","Beta8","Beta9","Beta10",
          "Beta11","Beta12","Beta13","Beta14","Beta15","Beta16","Beta17","Beta18","Beta19","Beta20")
          LabelParTheta=LabelParThetaaux[1:(2+pp)]
          }
 }
 if (model=="GGamma"){     #Begin GGamma
           if(is.null(Xt)){
          LabelParTheta=c("w","nu","chi")
    }else{
          pp=dim(Xt)[2]
          LabelParThetaaux=c("w","nu","chi","Beta1","Beta2","Beta3","Beta4","Beta5","Beta6","Beta7","Beta8","Beta9","Beta10",
          "Beta11","Beta12","Beta13","Beta14","Beta15","Beta16","Beta17","Beta18","Beta19","Beta20")
          LabelParTheta=LabelParThetaaux[1:(3+pp)]
          }
 }
  if (model=="Weibull"){   #Begin GED/Gamma/Weibull
           if(is.null(Xt)){
          LabelParTheta=c("w","nu")
    }else{
          pp=dim(Xt)[2]
          LabelParThetaaux=c("w","nu","Beta1","Beta2","Beta3","Beta4","Beta5","Beta6","Beta7","Beta8","Beta9","Beta10",
          "Beta11","Beta12","Beta13","Beta14","Beta15","Beta16","Beta17","Beta18","Beta19","Beta20")
          LabelParTheta=LabelParThetaaux[1:(2+pp)]
          }
 }

}#EndDefaultLabel
#############################################################################################


#if(length(LabelParTheta)!=length(StaPar))stop("Bad input for LabelParTheta for this model")
if(verbose) cat ("\n*****Non-Gaussian State Space Models with Exact Likelihood*****\n",
"\nNGSSEML Package:","Bayes -",model,"\n")
#cat("\nTime:.")
message("\nPosterior inputs...")
message("\nDone!")
#cat("\nTime:..")
message("\n")
message("\nPosterior computations...")
message("\n")
set.seed(1000)

#############################################################################################
if ((is.null(LabelParTheta)==FALSE)){
 if(is.null(StaPar)==FALSE){
    if(length(LabelParTheta)!=length(StaPar))stop("Bad input for LabelParTheta for this model")
 }
}
# If default of StaPar is true, initialize StaPar.  
if(is.null(StaPar)==FALSE){
StaPar1=StaPar;
#Begin Default StarPar
StaPar1[1]=log(-log(StaPar[1]));
if (model=="GED" | model=="Gamma" | model=="Weibull"){   #Begin GED/Gamma/Weibull 
StaPar1[2]=log(StaPar[2]);
};
if (model=="GGamma"){   #Begin GGamma 
StaPar1[2]=log(StaPar[2]);StaPar1[3]=log(StaPar[3]);
};
};

if(is.null(StaPar)){
if (model=="Poisson"){ #Begin Poisson
##StaPar:
if(is.null(Xt)){dxt=0}else{dxt=dim(Xt)[2];}
if(is.null(Zt)){dzt=0}else{dzt=0;}
p=(1+dxt+0);
StaPar=numeric(p);StaPar1=StaPar;
StaPar[1]=0.8;StaPar1[1]=log(-log(StaPar[1]));if(dxt>0){StaPar[2:(dxt+1)]=rep(0,dxt);};
##lower:
if(is.null(lower)){
lower=numeric(p);
blbeta=-100;
bubeta=100;
lower[1]=0.001;if(dxt>0){lower[2:(dxt+1)]=rep(blbeta,dxt);};
};
##upper:
if(is.null(upper)){
upper=numeric(p);
blmu=-100;
bumu=100;
upper[1]=0.999;if(dxt>0){upper[2:(dxt+1)]=rep(bumu,dxt);};
};
} #End Poisson

if (model=="Normal"){  #Begin Normal
##StaPar:
if(is.null(Xt)){dxt=0}else{dxt=dim(Xt)[2];}
if(is.null(Zt)){dzt=0}else{dzt=dim(Zt)[2];}
p=(1+dxt+dzt);
StaPar=numeric(p);StaPar1=StaPar;
StaPar[1]=0.8;StaPar1[1]=log(-log(StaPar[1]));if(dxt>0){StaPar[2:(dxt+1)]=rep(0,dxt);};if(dzt>0){StaPar[(dxt+1):(dxt+1+dzt)]=rep(0,dzt);};
##lower:
if(is.null(lower)){
lower=numeric(p);
blbeta=-100;
bubeta=100;
lower[1]=0.001;if(dxt>0){lower[2:(dxt+1)]=rep(blbeta,dxt);};if(dzt>0){lower[(dxt+1):(dxt+1+dzt)]=rep(blbeta,dzt);};
};
##upper:
if(is.null(upper)){
upper=numeric(p);
blmu=-100;
bumu=100;
upper[1]=0.999;if(dxt>0){upper[2:(dxt+1)]=rep(bumu,dxt);};if(dzt>0){upper[(dxt+1):(dxt+1+dzt)]=rep(bumu,dzt);};
};
}#End Normal

if (model=="Laplace"){  #Begin Laplace
##StaPar:
if(is.null(Xt)){dxt=0}else{dxt=dim(Xt)[2];}
if(is.null(Zt)){dzt=0}else{dzt=dim(Zt)[2];}
p=(1+dxt+dzt);
StaPar=numeric(p);StaPar1=StaPar;
StaPar[1]=0.8;StaPar1[1]=log(-log(StaPar[1]));if(dxt>0){StaPar[2:(dxt+1)]=rep(0,dxt);};if(dzt>0){StaPar[(dxt+1):(dxt+1+dzt)]=rep(0,dzt);};
##lower:
if(is.null(lower)){
lower=numeric(p);
blbeta=-100;
bubeta=100;
lower[1]=0.001;if(dxt>0){lower[2:(dxt+1)]=rep(blbeta,dxt);};if(dzt>0){lower[(dxt+1):(dxt+1+dzt)]=rep(blbeta,dzt);};
};
##upper:
if(is.null(upper)){
upper=numeric(p);
blmu=-100;
bumu=100;
upper[1]=0.999;if(dxt>0){upper[2:(dxt+1)]=rep(bumu,dxt);};if(dzt>0){upper[(dxt+1):(dxt+1+dzt)]=rep(bumu,dzt);};
};
} #End Laplace

if (model=="GED"){   #Begin GED
##StaPar:
if(is.null(Xt)){dxt=0}else{dxt=dim(Xt)[2];}
if(is.null(Zt)){dzt=0}else{dzt=dim(Zt)[2];}
p=(1+1+dxt+dzt);
StaPar=numeric(p); StaPar1=StaPar;
StaPar[1]=0.9;StaPar1[1]=log(-log(StaPar[1]));StaPar[2]=1;StaPar1[2]=log(StaPar[2]);
if(dxt>0){StaPar[3:(dxt+2)]=rep(0,dxt);};if(dzt>0){StaPar[(dxt+3):(dxt+2+dzt)]=rep(0,dzt);};
##lower:
if(is.null(lower)){
lower=numeric(p);
blbeta=-10;
bubeta=10;
lower[1]=0.001;lower[2]=0.001;if(dxt>0){lower[3:(dxt+2)]=rep(blbeta,dxt);};if(dzt>0){lower[(dxt+3):(dxt+2+dzt)]=rep(blbeta,dzt);};
};
##upper:
if(is.null(upper)){
upper=numeric(p);
blmu=-10;
bumu=10;
upper[1]=0.999;upper[2]=1000;if(dxt>0){upper[3:(dxt+2)]=rep(bumu,dxt);};if(dzt>0){upper[(dxt+3):(dxt+2+dzt)]=rep(bumu,dzt);};
};

}#End GED

if (model=="Gamma"){     #Begin Gamma
##StaPar:
if(is.null(Xt)){dxt=0}else{dxt=dim(Xt)[2];}
if(is.null(Zt)){dzt=0}else{dzt=dim(Zt)[2];}
p=(1+1+dxt+dzt);
StaPar=numeric(p);StaPar1=StaPar;
StaPar[1]=0.8;StaPar1[1]=log(-log(StaPar[1]));StaPar[2]=1;StaPar1[2]=log(StaPar[2]);if(p>2){StaPar[3:(dxt+2)]=rep(0,dxt);};
##lower:
if(is.null(lower)){
lower=numeric(p);
blbeta=-1000;
bubeta=1000;
lower[1]=0.001;lower[2]=0.001;if(p>2){lower[3:(dxt+2)]=rep(blbeta,dxt);};
};
##upper:
if(is.null(upper)){
upper=numeric(p);
blmu=-1000;
bumu=1000;
upper[1]=0.999;upper[2]=1000;if(p>2){upper[3:(dxt+2)]=rep(bumu,dxt);};
};

}#End Gamma

if (model=="GGamma"){     #Begin GGamma
##StaPar:
if(is.null(Xt)){dxt=0}else{dxt=dim(Xt)[2];}
if(is.null(Zt)){dzt=0}else{dzt=dim(Zt)[2];}
p=(1+1+dxt+dzt);
StaPar=numeric(p);StaPar1=StaPar;
StaPar[1]=0.8;StaPar1[1]=log(-log(StaPar[1]));StaPar[2]=1;StaPar1[2]=log(StaPar[2]);StaPar[3]=1;StaPar1[3]=log(StaPar[3]);if(p>3){StaPar[4:(dxt+2)]=rep(0,dxt);};
##lower:
if(is.null(lower)){
lower=numeric(p);
blbeta=-1000;
bubeta=1000;
lower[1]=0.001;lower[2]=0.001;lower[3]=0.001;if(p>3){lower[4:(dxt+2)]=rep(blbeta,dxt);};
};
##upper:
if(is.null(upper)){
upper=numeric(p);
blmu=-1000;
bumu=1000;
upper[1]=0.999;upper[2]=1000;upper[3]=1000;if(p>3){upper[4:(dxt+2)]=rep(bumu,dxt);};
};
}#End GGamma

if (model=="Weibull"){    #Begin Weibull
##StaPar:
if(is.null(Xt)){dxt=0}else{dxt=dim(Xt)[2];}
if(is.null(Zt)){dzt=0}else{dzt=dim(Zt)[2];}
p=(1+dxt+dzt);
StaPar=numeric(p);StaPar1=StaPar;
StaPar[1]=0.8;StaPar1[1]=log(-log(StaPar[1]));StaPar[2]=1;StaPar1[2]=log(StaPar[2]);if(p>2){StaPar[3:(dxt+2)]=rep(0,dxt);};
##lower:
if(is.null(lower)){
lower=numeric(p);
blbeta=-1000;
bubeta=1000;
lower[1]=0.001;lower[2]=0.001;if(p>2){lower[3:(dxt+2)]=rep(blbeta,dxt);};
};
##upper:
if(is.null(upper)){
upper=numeric(p);
blmu=-1000;
bumu=1000;
upper[1]=0.999;upper[2]=1000;if(p>2){upper[3:(dxt+2)]=rep(bumu,dxt);};
};

} #End Weibull

}#End Default StarPar
#############################################################################################

#############################
###GRID 
#############################

nn=length(Yt)
resultsopt=NA
resultsopt=tryCatch(optim(StaPar1, hessian=TRUE,LikeF2,
#lower=lower,upper=upper,
method="BFGS",na.action=na.action,Yt=Yt,Xt=Xt,Zt=Zt,Break=Break,Event=Event,
a0=a0,b0=b0,model=model,control = list(maxit = 30000, temp = 2000,
trace = FALSE,REPORT = 500)), error = c)
if(is.null(resultsopt$convergence)){stop("Convergence error! Bad inputs! Sorry!")}else{
if(resultsopt$convergence!=0)stop("Convergence error! Bad inputs! Sorry!")}
estopt=resultsopt$par     # Point Estimates:
estopt
Hessianmatrixopt=-resultsopt$hessian   #Hessian Matrix
Hessianmatrixopt
MIFopt=-solve(Hessianmatrixopt)       #MIF
MIFopt
MIFopt[1,1]=MIFopt[1,1]*(((exp(-exp(estopt[1])))*(-exp(estopt[1])))^2)
MIFopt
estopt[1]=exp(-exp(estopt[1]))
LI=estopt-1*(qnorm((1+ci)/2))*sqrt(diag(MIFopt))
LS=estopt+1*(qnorm((1+ci)/2))*sqrt(diag(MIFopt))

if (model=="Gamma"){
MIFopt[2,2]=MIFopt[2,2]*(((exp(estopt[2])))^2)
MIFopt
estopt[2]=exp((estopt[2]))
LI=estopt-1*(qnorm((1+ci)/2))*sqrt(diag(MIFopt))
LS=estopt+1*(qnorm((1+ci)/2))*sqrt(diag(MIFopt))
#LI[1]=exp(-exp(estopt[1]+1*(qnorm((1+ci)/2))*sqrt((MIFopt[1,1]))))
#LS[1]=exp(-exp(estopt[1]-1*(qnorm((1+ci)/2))*sqrt((MIFopt[1,1]))))
if(LI[2]<0){LI[2]=0}
}
if (model=="Weibull"){
MIFopt[2,2]=MIFopt[2,2]*(((exp(estopt[2])))^2)
MIFopt
estopt[2]=exp((estopt[2]))
LI=estopt-1*(qnorm((1+ci)/2))*sqrt(diag(MIFopt))
LS=estopt+1*(qnorm((1+ci)/2))*sqrt(diag(MIFopt))
#LI[1]=exp(-exp(estopt[1]+1*(qnorm((1+ci)/2))*sqrt((MIFopt[1,1]))))
#LS[1]=exp(-exp(estopt[1]-1*(qnorm((1+ci)/2))*sqrt((MIFopt[1,1]))))
if(LI[2]<0){LI[2]=0}
}
if (model=="GED"){
MIFopt[2,2]=MIFopt[2,2]*(((exp(estopt[2])))^2)
MIFopt
estopt[2]=exp((estopt[2]))
LI=estopt-1*(qnorm((1+ci)/2))*sqrt(diag(MIFopt))
LS=estopt+1*(qnorm((1+ci)/2))*sqrt(diag(MIFopt))
#LI[1]=exp(-exp(estopt[1]+1*(qnorm((1+ci)/2))*sqrt((MIFopt[1,1]))))
#LS[1]=exp(-exp(estopt[1]-1*(qnorm((1+ci)/2))*sqrt((MIFopt[1,1]))))
#if(LI[2]<0){LI[2]=0}
}
if (model=="GGamma"){
MIFopt[2,2]=MIFopt[2,2]*(((exp(estopt[2])))^2)
MIFopt[3,3]=MIFopt[3,3]*(((exp(estopt[3])))^2)
MIFopt
estopt[2:3]=exp((estopt[2:3]))
LI=estopt-1*(qnorm((1+ci)/2))*sqrt(diag(MIFopt))
LS=estopt+1*(qnorm((1+ci)/2))*sqrt(diag(MIFopt))
#LI[1]=exp(-exp(estopt[1]+1*(qnorm((1+ci)/2))*sqrt((MIFopt[1,1]))))
#LS[1]=exp(-exp(estopt[1]-1*(qnorm((1+ci)/2))*sqrt((MIFopt[1,1]))))
#if(LI[2]<0){LI[2]=0}
#if(LI[3]<0){LI[3]=0}
}

qn=3
p=length(StaPar1)
linf=numeric(p)
lsup=numeric(p)
for(jj in 1:p){   # Intervals for Grid
linf[jj]=estopt[jj]-qn*sqrt(MIFopt[jj,jj])
lsup[jj]=estopt[jj]+qn*sqrt(MIFopt[jj,jj])
}
lsup[1]=lsup[1]-0.0000001
if(linf[1]<0){linf[1]=1e-20}
#if(linf[1]>0.9){linf[1]=0.85}
if(lsup[1]>1){lsup[1]=0.99999999}

#Compute the Grid:=============================================================
##
## USING THE BUILDGRID_FUNCTION-2018
##
##=============================================================================
Delta=(lsup-linf)/(pointss-1)
grid_output=gridfunction(pointss,linf,lsup)  #Call the Grid function

#####Posterior ###########
Log.Post.rw<-function(StaPar,formula=formula, data=data,na.action=na.action,pz=pz,nBreaks=nBreaks){ 
log.post=-LikeF(StaPar,na.action=na.action,Yt=Yt,Xt=Xt,Zt=Zt,Break=Break,Event=Event,
model=model,a0=a0,b0=b0)+log(PriorF(StaPar,model=model,prw=prw,prnu=prnu,prchi=prchi,prmu=prmu,
prbetamu=prbetamu,prbetasigma=prbetasigma))
return(log.post)
}

np=dim(grid_output)[1]
if(is.null(np)){np=dim(t(t(grid_output)))[[1]]
grid_output=t(t(grid_output))}
#print(grid_output)
logpost=numeric(np)
#nn=1
SEQ  <- seq(1,np)
pb   <- txtProgressBar(1, np, style=3)
TIME <- Sys.time()
for (i in 1:np){
#if(i==1){
#cat("\n","\t","Iter.=",i)
#}else{cat("\t",i)}
logpost[i]=Log.Post.rw(grid_output[i,],
formula=formula, data=data,na.action=na.action,pz=pz,nBreaks=nBreaks)
Sys.sleep(0.02)
setTxtProgressBar(pb, i)
}

## ##estender ordem p! =========================================================
dpost=exp(logpost-mean(logpost)) #             standard post. values
#dpost=exp((logpost-mean(logpost))/sd(logpost)) #
constI=sum(prod(Delta)*dpost)
dpostvector=matrix(0,length(dpost),p)
modeest=numeric(p)
meanest=numeric(p)
sdest=numeric(p)
q1est=numeric(p)
q2est=numeric(p)
q3est=numeric(p)
quantilevector=matrix(0,3,p)
postsample=matrix(0,nsamplex,p)

for(ii in 1:p){
if(ii==1){
message("\nSummary.Calculation","\t","=",ii)
}else{cat("\t",ii)}

dpostvector[,ii]=(prod(Delta[-ii]))*(dpost/constI)
rw=aggregate(as.data.frame(dpostvector[,ii]), by=as.data.frame(grid_output[,ii]),FUN=sum)
#Mode:
#posw=which.max(rw[[2]])
#modeest[ii]=rw[[1]][posw]
################################################################################
##
##   EXACT MOMENTS USING NUMERICAL INTEGRATION
##
################################################################################
###
### Summaries for Each  Static Parameter:
###
#w:===============================================
#Mean:
meanest[ii]=sum(rw[[1]]*rw[[2]]*(Delta[ii]))
#medw
#Var:
sdest[ii]=sqrt(sum(rw[[2]]*(rw[[1]]^2)*(Delta[ii]))-meanest[ii]^2)
#varw
#Quantiles:
#linfw=medw-1.96*sqrt(varw)
#lsupw=medw+1.96*sqrt(varw)
#linfw
#lsupw
#cc=sum(rw[[2]]*(Delta[ii]))
conf=ci
alpha1=(1-conf)/2
alpha2=(1+conf)/2
cc=(rw[[2]]*(Delta[ii]))/sum(rw[[2]]*(Delta[ii]))
#cc=y*h
#Linf:
csy=cumsum(cc)
#cat("\n",csy,"grid=",grid_output[,ii])
csyt1=csy[csy<=alpha1]

#posq1=table(csy<=alpha1)[[2]]
if(length(table(csy<=alpha1))==1){posq1=1+1}
if(length(table(csy<=alpha1))==2){posq1=table(csy<=alpha1)[[2]]}

#cat(grid_output[posq1,ii],csy[posq1])
#q1est[ii]=grid_output[posq1,ii]
#q1est[ii]=rw[[1]][posq1]
if(csy[posq1]>alpha1){
#q1est[ii]=mean(c(rw[[1]][posq1],rw[[1]][posq1-1]))
sw=csy[posq1]+csy[posq1-1]
q1est[ii]=((csy[posq1]/sw)*rw[[1]][posq1]+(csy[posq1-1]/sw)*rw[[1]][posq1-1])
}else{
#q1est[ii]=mean(c(rw[[1]][posq1],rw[[1]][posq1+1]))
sw=csy[posq1]+csy[posq1+1]
q1est[ii]=((csy[posq1]/sw)*rw[[1]][posq1]+(csy[posq1+1]/sw)*rw[[1]][posq1+1])
}

#Median:
csyt2=csy[csy<=0.50]

#posq2=table(csy<=0.50)[[2]]
if(length(table(csy<=0.5))==1){posq2=1+1}
if(length(table(csy<=0.5))==2){posq2=table(csy<=0.5)[[2]]}

#cat(grid_output[posq2,ii],csy[posq2])
#q2est[ii]=grid_output[posq2,ii]
#q2est[ii]=rw[[1]][posq2]
if(csy[posq2]>0.5){
#q2est[ii]=mean(c(rw[[1]][posq2],rw[[1]][posq2-1]))
sw=csy[posq2]+csy[posq2-1]
q2est[ii]=((csy[posq2]/sw)*rw[[1]][posq2]+(csy[posq2-1]/sw)*rw[[1]][posq2-1])
}else{
#q2est[ii]=mean(c(rw[[1]][posq2],rw[[1]][posq2+1]))
sw=csy[posq2]+csy[posq2+1]
q2est[ii]=((csy[posq2]/sw)*rw[[1]][posq2]+(csy[posq2+1]/sw)*rw[[1]][posq2+1])
}

#Lsup:
csyt3=csy[csy<=alpha2]

#posq3=table(csy<=alpha2)[[2]]
if(length(table(csy<=alpha2))==1){posq3=1+1}
if(length(table(csy<=alpha2))==2){posq3=table(csy<=alpha2)[[2]]}

#q3est[ii]=grid_output[posq3,ii]
#q3est[ii]=rw[[1]][posq3]
if(csy[posq3]>alpha2){
#q3est[ii]=mean(c(rw[[1]][posq3],rw[[1]][posq3-1]))
sw=csy[posq3]+csy[posq3-1]
q3est[ii]=((csy[posq3]/sw)*rw[[1]][posq2]+(csy[posq3-1]/sw)*rw[[1]][posq3-1])
}else{
#q3est[ii]=mean(c(rw[[1]][posq3],rw[[1]][posq3+1]))
sw=csy[posq3]+csy[posq3+1]
q3est[ii]=((csy[posq3]/sw)*rw[[1]][posq3]+(csy[posq3+1]/sw)*rw[[1]][posq3+1])
}
#cat(grid_output[posq3,ii],csy[posq3])
#Mode:
posw=which.max((rw[[2]]))
modeest[ii]=rw[[1]][posw]
################################################################################

## Quantiles and Mean calculate using a sample of marginal posterior distribution:
xw <- sample(rw[[1]],nsamplex,replace=TRUE,prob=rw[[2]])
postsample[,ii]=xw 
#cat("\nSummaries:", summary(xw))
#meanest[ii]=mean(xw)
#cat("\nQuantiles:",quantile(xw,probs=c(alpha/2,0.5,1-alpha/2)))
#alpha=1-ci
#quantilevector[,ii]=quantile(xw,probs=c(alpha/2,0.5,1-alpha/2))

}
message("\nDone!")
message("\nTime:...\n")
#mfit<-matrix(c(modeest,meanest,t(quantilevector[1,]),t(quantilevector[2,]),t(quantilevector[3,])),p,5)
#colnames(mfit)=c("Mode","Mean","LI","Median","LS")
mfit<-matrix(c(meanest,q2est,sdest,q1est,q3est),p,5)
colnames(mfit)=c("Mean","Median","Sd","Lower","Upper")
rownames(mfit)=paste0(c('\u03b8'),1:p)
if(is.null(LabelParTheta)==FALSE){rownames(mfit)=LabelParTheta}
nn=length(Yt)
ngssm.list<-list(mfit,ci*100,nn,meanest)
names(ngssm.list)<-c("Bayesian Estimation","Nom. Level(%)","n.obs","coefficients")
#cat("\n*****Non-Gaussian State Space Models with Exact Likelihood*****\n",
#"\nNGSSMEL Package:","Bayes -",model,"\n") 
if(verbose) print (ngssm.list)
#names(ngssm.list)<-c("\n*****Non-Gaussian State Space Models with Exact Likelihood*****\n",
#"\nNGSSMEL Package:","Bayes -",model,"Cred. Level(%)","n.obs")
#ngssm.list<-list(mfit,ci*100,nn,meanest)
#ngssm.list<-list(mfit,ci*100,nn,meanest)
#names(ngssm.list)<-c("Bayesian Estimation","Nom. Level(%)","n.obs","coefficients")
ngssm.list=list(ngssm.list,postsample)
p=length(StaPar)
if(postplot==TRUE){
# dev.new()
message("\nMarginal posterior graphs...")
message("\nTime:....\n")
oldpar <- par(mar=rep(2, 4),mfrow=c(p,2))
on.exit(par(oldpar)) #line i+1
#par(mar=rep(2, 4))
#par(mfrow=c(p,2))
for(iii in 1:p){
if(iii==1){
message("\n","\t","Graph=",iii)
}else{cat("\t",iii)}
xlabbp=paste0(c('\u03b8'),iii)
if(is.null(LabelParTheta)==FALSE){xlabbp=LabelParTheta[iii]}
rb1=aggregate(as.data.frame(dpostvector[,iii]), by=as.data.frame(grid_output[,iii]),FUN=sum)
plot(round(rb1[[1]],3),round(rb1[[2]],3),type='l',xlim=c(linf[iii],lsup[iii]),xlab=xlabbp,ylab="",axes=F,main=xlabbp)
lines(c(meanest[iii],meanest[iii]),c(min(rb1),max(rb1)),lwd=2,col="red") # posterior mean
#lines(c(modeest[iii],modeest[iii]),c(min(rb1),max(rb1)),lwd=2,col="red") # posterior mode
lines(c(q1est[iii],q1est[iii]),c(min(rb1),(max(rb1)-min(rb1))/3),lwd=2,col="blue",lty=c(2)) # linf
lines(c(q3est[iii],q3est[iii]),c(min(rb1),(max(rb1)-min(rb1))/3),lwd=2,col="blue",lty=c(2)) # lsup
axis(1,round(rb1[[1]],4))
legend("topleft", c("Posterior Mean","Cred.Int"),col=c("red","blue"),lty=c(1,2),bty = "n")
}
message("\nDone!\n")
}#end post plot
#,contourplot=FALSE
if(p==1 && contourplot==TRUE){contourplot=FALSE;
warning("\n","It is not possible to build countour plot with p=1!")
}
#,contourplot=FALSE
if(contourplot==TRUE){
message("\nContour posterior graphs...")
message("\nTime:.....\n")
##########################################################
##### Building Lower Triangular Matrix
#p=3
if(p==1){
warning("\n","It is not possible to build countour plot with p=1!")
}else{
numgraphs=choose(p,2)+p
vii=numeric(numgraphs)
vj=numeric(numgraphs)
comp=0
for(jjj in 2:p){
iii=1
if(jjj==2){
message("\n","\t","Graph=",jjj-1)
}else{message("\t",jjj-1)}
while((iii<jjj)&&(iii!=jjj)){
#cat("\n",iii,jjj,comp+1)
vii[comp+1]=iii
vj[comp+1]=jjj
iii=iii+1
comp=comp+1
}
}
}
#vii
#vj
if(p==1){vii=vj=1}
pend=choose(p,2)+1
vii[pend:(numgraphs)]=10000
vj[pend:(numgraphs)]=10000
viij=data.frame(vii,vj)
viij
viijs=viij[order(viij$vii),]
viis=viijs$vii
viis
vjs=viijs$vj
vjs
viis[pend:(numgraphs)]=1:p
vjs[pend:(numgraphs)]=1:p

##########################################################
numgraphs=choose(p,2)+p
mpost=matrix(logpost)
#akima1<-0
#akima<-0
#data(akima)
#assign("akima","new", envir = .GlobalEnv)
#assign(akima,data(akima), envir = .GlobalEnv)
#akima1=akima
#akima1<-0
#akima<-0

##data(akima)
##assign("akima", akima, envir = .GlobalEnv)
##akima1=akima
mpost=matrix(logpost)
#par(mar=rep(1, 4))
#par(mfrow=c(numgraphs,2))
#dev.new()
oldpar <- par(mar=c(5,5,1,1))
on.exit(par(oldpar)) #line i+1
#par(mar=c(5,5,1,1))
#par(mfrow=c(p-1,p-1))
#if(p==2){m=matrix(c(1:((p-1)*(p-1))),p-1,p-1,byrow=TRUE)
#}else{
m=matrix(c(1:((p)*(p))),p,p,byrow= TRUE)
#}
ltm=lower.tri(m)
#if(p==2){
xlay=rep(0,(p-1)*(p-1))
#}else{
xlay=rep(0,p*p)
#}
auxp=m[ltm]
numbergraph=choose(p,2)
auxnumbergraph=1:(numbergraph)

xlay[auxp]=auxnumbergraph

#if(p==2){layout(c(xlay+1))
#}else{
ma=matrix(xlay, p, p, byrow = TRUE)
maa=ma
for(kk in 1:p){maa[kk,kk]=choose(p,2)+kk
#}
layout(maa)
}

for(jjj in 1:numgraphs){

#par(c(vii[jjj],vii[jjj]))
#contour plot
xlabbx=paste0(c('\u03b8'),viis[jjj])
xlabby=paste0(c('\u03b8'),vjs[jjj])
if(is.null(LabelParTheta)==FALSE){xlabbx=LabelParTheta[viis[jjj]]
xlabby=LabelParTheta[vjs[jjj]]
}

if(viis[jjj]!=vjs[jjj]){
#vii[jjj]
#vj[jjj]
#beta2=read.table("resultlikdelta.csv",sep=";",dec=",") 
#w1=read.table("resultlikw.csv",sep=";",dec=",")
#persp(grid_output[,vii[jjj]],grid_output[,vj[jjj]],(dpostvector[,vj[jjj]),theta=45,phi=30,expand=0.7,col = "lightblue",
#ltheta = 120, shade = 0.75, ticktype = "detailed",xlab = "w", ylab = "Beta", zlab = "Post")

fld <- with(list(round(grid_output[,viis[jjj]],3),round(grid_output[,vjs[jjj]],3),log(dpost)),
interp(round(grid_output[,viis[jjj]],3),round(grid_output[,vjs[jjj]],3),log(dpost),duplicate="mean"))

contour(fld,drawlabels = FALSE,col = "black",xlab = xlabbx, ylab = xlabby, method = "edge",nlevels=15)
#abline(h=mean(vii[jjj]), v=mean(vj[jjj]), lwd=1)
#points(c(meanest[vii[jjj]]),c(meanest[vj[jjj]]),col="red",lwd=c(2),pch=16)
#legend("topleft", c("Posterior Mean"),col="red",pch=16,bty = "n")

#legend("topleft", c("Posterior Mode"),col="red",pch=16,bty = "n")
#points(c(modeest[vii[jjj]]),c(modeest[vj[jjj]]),col="red",lwd=c(2),pch=16,bty = "n")

#if(jjj==1){title("Contour Plots")}
#image(fld)
#persp((grid_output[,vii[jjj]]),(grid_output[,vj[jjj]]),logpost,
#drawlabels = FALSE,col = "black",xlab = "w", ylab = "Beta", method = "edge")
}



if(postplot==TRUE){
if(viis[jjj]==vjs[jjj]){
if(viis[jjj]==1){
message("\n","\t","Graph=",viis[jjj])
}else{message("\t",viis[jjj])}
xlabbp=paste0(c('\u03b8'),viis[jjj])
if(is.null(LabelParTheta)==FALSE){xlabbp=LabelParTheta[viis[jjj]]}
rb1=aggregate(as.data.frame(dpostvector[,viis[jjj]]), by=as.data.frame(grid_output[,viis[jjj]]),FUN=sum)
plot(round(rb1[[1]],3),round(rb1[[2]],3),type='l',xlim=c(linf[viis[jjj]],lsup[viis[jjj]]),xlab=xlabbp,ylab="",axes=F,main=xlabbp)
lines(c(round(meanest[viis[jjj]],3),round(meanest[viis[jjj]],3)),c(min(round(rb1,3)),max(round(rb1,3))),lwd=2,col="red") # posterior mean
#lines(c(modeest[iii],modeest[iii]),c(min(rb1),max(rb1)),lwd=2,col="red") # posterior mode
lines(c(round(q1est[viis[jjj]],3),round(q1est[viis[jjj]],3)),c(min(round(rb1,3)),(max(round(rb1,3))-min(round(rb1,3)))/3),lwd=2,col="blue",lty=c(2)) # linf
lines(c(round(q3est[viis[jjj]],3),round(q3est[viis[jjj]],3)),c(min(round(rb1,3)),(max(round(rb1,3))-min(round(rb1,3)))/3),lwd=2,col="blue",lty=c(2)) # lsup
axis(1,round(rb1[[1]],3))
legend("topright", c("Posterior Mean","Cred.Int"),col=c("red","blue"),lty=c(1,2),bty = "n")
}
}

}

message("\nDone!\n")

} # end contour plot


message("End!")
#ngssm.list[[1]]
#return(ngssm.list)

}

if(model=="SRGamma" || model=="SRWeibull"){                #SR
#if(model=="SRGamma"){model1="Gamma"}
#if(model=="SRWeibull"){model1="Weibull"}
if (is.null(Event)==FALSE)stop("Bad input Event for this model")
if (is.null(Break)==FALSE)stop("Bad input Break for this model")
if (amp==TRUE)stop("Bad input for amp for this model")
if(is.null(Zt)==FALSE)stop("Bad input for Zt for this model")
if(verbose) cat ("\n*****Non-Gaussian State Space Models with Exact Likelihood*****\n",
"\nNGSSEML Package:","Bayes -",model,"\n")
#cat("\nTime:.")
message("\nPosterior inputs...")
message("\nDone!")
#cat("\nTime:..")
message("\n")
message("\nPosterior computations...")
message("\n")
set.seed(1000)

#############################################################################################
if(is.null(LabelParTheta)){   #BeginDefaultLabel
if (model=="SRGamma"){ #Begin Poisson
    if(is.null(Xt)){
          LabelParTheta=c("w","nu")
    }else{
          pp=dim(Xt)[2]
          LabelParThetaaux=c("w","nu","Beta1","Beta2","Beta3","Beta4","Beta5","Beta6","Beta7","Beta8","Beta9","Beta10",
          "Beta11","Beta12","Beta13","Beta14","Beta15","Beta16","Beta17","Beta18","Beta19","Beta20")
          LabelParTheta=LabelParThetaaux[1:(2+pp)]
          }

}
if (model=="SRWeibull"){ #Begin Poisson
    if(is.null(Xt)){
          LabelParTheta=c("w","nu")
    }else{
          pp=dim(Xt)[2]
          LabelParThetaaux=c("w","nu","Beta1","Beta2","Beta3","Beta4","Beta5","Beta6","Beta7","Beta8","Beta9","Beta10",
          "Beta11","Beta12","Beta13","Beta14","Beta15","Beta16","Beta17","Beta18","Beta19","Beta20")
          LabelParTheta=LabelParThetaaux[1:(2+pp)]
          }

}
}#EndDefaultLabel
#############################################################################################

#############################################################################################
if ((is.null(LabelParTheta)==FALSE)){
 if(is.null(StaPar)==FALSE){
    if(length(LabelParTheta)!=length(StaPar))stop("Bad input for LabelParTheta for this model")
 }
}
# If default of StaPar is true, initialize StaPar.
#Begin Default StarPar
if(is.null(StaPar)==FALSE){
StaPar1=StaPar;
StaPar1[1]=log(-log(StaPar[1]));
StaPar1[2]=log(StaPar[2]);
};
if(is.null(StaPar)){
if (model=="SRGamma"){     #Begin Gamma
##StaPar:
if(is.null(Xt)){dxt=0}else{dxt=dim(Xt)[2];}
if(is.null(Zt)){dzt=0}else{dzt=dim(Zt)[2];}
p=(1+1+dxt+dzt);
StaPar=numeric(p);StaPar1=StaPar;
StaPar[1]=0.8;StaPar1[1]=log(-log(StaPar[1]));StaPar[2]=1;StaPar1[2]=log(StaPar[2]);if(p>2){StaPar[3:(dxt+2)]=rep(0,dxt);};
##lower:
if(is.null(lower)){
lower=numeric(p);
blbeta=-5;
bubeta=5;
lower[1]=0.001;lower[2]=0.001;if(p>2){lower[3:(dxt+2)]=rep(blbeta,dxt);};
};
##upper:
if(is.null(upper)){
upper=numeric(p);
blmu=-5;
bumu=5;
upper[1]=0.999;upper[2]=1000;if(p>2){upper[3:(dxt+2)]=rep(bumu,dxt);};
};

}#End Gamma

if (model=="SRWeibull"){    #Begin Weibull
##StaPar:
if(is.null(Xt)){dxt=0}else{dxt=dim(Xt)[2];}
if(is.null(Zt)){dzt=0}else{dzt=dim(Zt)[2];}
p=(1+1+dxt+dzt);
StaPar=numeric(p);StaPar1=StaPar;
StaPar[1]=0.8;StaPar1[1]=log(-log(StaPar[1]));StaPar[2]=1;StaPar1[2]=log(StaPar[2]);if(p>2){StaPar[3:(dxt+2)]=rep(0,dxt);};
##lower:
if(is.null(lower)){
lower=numeric(p);
blbeta=-5;
bubeta=5;
lower[1]=0.001;lower[2]=0.001;if(p>2){lower[3:(dxt+2)]=rep(blbeta,dxt);};
};
##upper:
if(is.null(upper)){
upper=numeric(p);
blmu=-5;
bumu=5;
upper[1]=0.999;upper[2]=1000;if(p>2){upper[3:(dxt+2)]=rep(bumu,dxt);};
};
} #End Weibull

}#End Default StarPar
#############################################################################################

################################################################################
##
## Weibull Example 
##
################################################################################

#############################
###GRID
#############################
nn=length(Yt)
resultsopt=NA
resultsopt=tryCatch(optim(StaPar1, hessian=TRUE,LikeF2,
#lower=lower,upper=upper,
method="BFGS",na.action=na.action,Yt=Yt,Xt=Xt,Zt=Zt,Break=Break,Event=Event,
a0=a0,b0=b0,model=model,control = list(maxit = 30000, temp = 2000,
trace = FALSE,REPORT = 500)), error = c)
if(is.null(resultsopt$convergence)){stop("Convergence error! Bad inputs! Sorry!")}else{
if(resultsopt$convergence!=0)stop("Convergence error! Bad inputs! Sorry!")}
estopt=resultsopt$par     # Point Estimates:
estopt
Hessianmatrixopt=-resultsopt$hessian   #Hessian Matrix
Hessianmatrixopt
MIFopt=-solve(Hessianmatrixopt)       #MIF
MIFopt

MIFopt[1,1]=MIFopt[1,1]*(((exp(-exp(estopt[1])))*(-exp(estopt[1])))^2)
MIFopt[2,2]=MIFopt[2,2]*(((exp(estopt[2])))^2)
MIFopt
estopt[1]=exp(-exp(estopt[1]))
estopt[2]=exp((estopt[2]))
LI=estopt-1*(qnorm((1+ci)/2))*sqrt(diag(MIFopt))
LS=estopt+1*(qnorm((1+ci)/2))*sqrt(diag(MIFopt))


qn=3
p=length(StaPar1)
linf=numeric(p)
lsup=numeric(p)
for(jj in 1:p){   # Intervals for Grid
linf[jj]=estopt[jj]-qn*sqrt(MIFopt[jj,jj])
lsup[jj]=estopt[jj]+qn*sqrt(MIFopt[jj,jj])
}
lsup[1]=lsup[1]-0.0000001
if(linf[1]<0){linf[1]=1e-20}
if(linf[1]>0.95){linf[1]=0.80}
if(lsup[1]>1){lsup[1]=0.999999999}
if(linf[2]<0){linf[2]=1e-20}
#print(linf)
#print(lsup)

#Compute the Grid:=============================================================
##
## USING THE BUILDGRID_FUNCTION-2018
##
##=============================================================================
Delta=(lsup-linf)/(pointss-1)
grid_output=gridfunction(pointss,linf,lsup)  #Call the Grid function
Log.Post.rw.sr<-function(StaPar,formula=formula, data=data,na.action=na.action,pz=pz,nBreaks=nBreaks){ 
log.post=-LikeF(StaPar,na.action=na.action,Yt=Yt,Xt=Xt,Zt=Zt,Break=Break,Event=Event,
model=model,a0=a0,b0=b0)+log(PriorF(StaPar,model=model,prw=prw,prnu=prnu,prchi=prchi,prmu=prmu,
prbetamu=prbetamu,prbetasigma=prbetasigma))
return(log.post)
 }

np=dim(grid_output)[1]
if(is.null(np)){np=dim(t(t(grid_output)))[[1]]
grid_output=t(t(grid_output))}
logpost=numeric(np)
SEQ  <- seq(1,np)
pb   <- txtProgressBar(1, np, style=3)
TIME <- Sys.time()
for (i in 1:np){
#if(i==1){
#cat("\n","\t","Iter.=",i)
#}else{cat("\t",i)}
logpost[i]=Log.Post.rw.sr(grid_output[i,],
formula=formula, data=data,na.action=na.action,pz=pz,nBreaks=nBreaks)
 Sys.sleep(0.02)
   setTxtProgressBar(pb, i)
}

## ##estender ordem p! =========================================================
dpost=exp(logpost-mean(logpost)) #       standard post. values
#dpost=exp((logpost-mean(logpost))/sd(logpost)) #
constI=sum(prod(Delta)*dpost)
dpostvector=matrix(0,length(dpost),p)
modeest=numeric(p)
meanest=numeric(p)
sdest=numeric(p)
q1est=numeric(p)
q2est=numeric(p)
q3est=numeric(p)
postsample=matrix(0,nsamplex,p)
quantilevector=matrix(0,3,p)

for(ii in 1:p){
if(ii==1){
message("\nSummary.Calculation","\t","=",ii)
}else{message("\t",ii)}
dpostvector[,ii]=(prod(Delta[-ii]))*(dpost/constI)
rw=aggregate(as.data.frame(dpostvector[,ii]), by=as.data.frame(grid_output[,ii]),FUN=sum)
#Mode:
#posw=which.max(rw[[2]])
#modeest[ii]=rw[[1]][posw]
################################################################################
##
##   EXACT MOMENTS USING NUMERICAL INTEGRATION
##
################################################################################
###
### Summaries for Each  Static Parameter:
###
#w:===============================================
#Mean:
meanest[ii]=sum(rw[[1]]*rw[[2]]*(Delta[ii]))
#medw
#Var:
sdest[ii]=sqrt(sum(rw[[2]]*(rw[[1]]^2)*(Delta[ii]))-meanest[ii]^2)
#varw
#Quantiles:
#linfw=medw-1.96*sqrt(varw)
#lsupw=medw+1.96*sqrt(varw)
#linfw
#lsupw
#cc=sum(rw[[2]]*(Delta[ii]))
conf=ci
alpha1=(1-conf)/2
alpha2=(1+conf)/2
cc=(rw[[2]]*(Delta[ii]))/sum(rw[[2]]*(Delta[ii]))
#cc=y*h
#Linf:
csy=cumsum(cc)
csyt1=csy[csy<=alpha1]
#cat("csy=",csy)

#posq1=table(csy<=alpha1)[[2]]
if(length(table(csy<=alpha1))==1){posq1=1+1}
if(length(table(csy<=alpha1))==2){posq1=table(csy<=alpha1)[[2]]}

#cat(grid_output[posq1,ii],csy[posq1])
#q1est[ii]=rw[[1]][posq1]
if(csy[posq1]>alpha1){
#q1est[ii]=mean(c(rw[[1]][posq1],rw[[1]][posq1-1]))
sw=csy[posq1]+csy[posq1-1]
q1est[ii]=((csy[posq1]/sw)*rw[[1]][posq1]+(csy[posq1-1]/sw)*rw[[1]][posq1-1])
}else{
#q1est[ii]=mean(c(rw[[1]][posq1],rw[[1]][posq1+1]))
sw=csy[posq1]+csy[posq1+1]
q1est[ii]=((csy[posq1]/sw)*rw[[1]][posq1]+(csy[posq1+1]/sw)*rw[[1]][posq1+1])
}

#Median:
csyt2=csy[csy<=0.50]
#posq2=table(csy<=0.50)[[2]]
if(length(table(csy<=0.5))==1){posq2=1+1}
if(length(table(csy<=0.5))==2){posq2=table(csy<=0.5)[[2]]}

#cat(grid_output[posq2,ii],csy[posq2])
if(csy[posq2]>0.5){
#q2est[ii]=mean(c(rw[[1]][posq2],rw[[1]][posq2-1]))
sw=csy[posq2]+csy[posq2-1]
q2est[ii]=((csy[posq2]/sw)*rw[[1]][posq2]+(csy[posq2-1]/sw)*rw[[1]][posq2-1])
}else{
#q2est[ii]=mean(c(rw[[1]][posq2],rw[[1]][posq2+1]))
sw=csy[posq2]+csy[posq2+1]
q2est[ii]=((csy[posq2]/sw)*rw[[1]][posq2]+(csy[posq2+1]/sw)*rw[[1]][posq2+1])
}
#Lsup:
csyt3=csy[csy<=alpha2]
#posq3=table(csy<=alpha2)[[2]]
if(length(table(csy<=alpha2))==1){posq3=1+1}
if(length(table(csy<=alpha2))==2){posq3=table(csy<=alpha2)[[2]]}

if(csy[posq3]>alpha2){
#q3est[ii]=mean(c(rw[[1]][posq3],rw[[1]][posq3-1]))
sw=csy[posq3]+csy[posq3-1]
q3est[ii]=((csy[posq3]/sw)*rw[[1]][posq2]+(csy[posq3-1]/sw)*rw[[1]][posq3-1])
}else{
#q3est[ii]=mean(c(rw[[1]][posq3],rw[[1]][posq3+1]))
sw=csy[posq3]+csy[posq3+1]
q3est[ii]=((csy[posq3]/sw)*rw[[1]][posq3]+(csy[posq3+1]/sw)*rw[[1]][posq3+1])
}
#cat(grid_output[posq3,ii],csy[posq3])
#Mode:
posw=which.max((rw[[2]]))
modeest[ii]=rw[[1]][posw]
################################################################################

## Quantiles and Mean calculate using a sample of marginal posterior distribution:
xw <- sample(rw[[1]],nsamplex,replace=TRUE,prob=rw[[2]]) 
postsample[,ii]=xw
#cat("\nPosterior computations...")
message("\nDone!")
message("\nTime:...\n")
#cat("\nSummaries:", summary(xw))
#meanest[ii]=mean(xw)
#alpha=1-ci
#cat("\nQuantiles:",quantile(xw,probs=c(alpha/2,0.5,1-alpha/2)))
#quantilevector[,ii]=quantile(xw,probs=c(alpha/2,0.5,1-alpha/2))
}

#mfit<-matrix(c(modeest,meanest,t(quantilevector[1,]),t(quantilevector[2,]),t(quantilevector[3,])),p,5)
#colnames(mfit)=c("Mode","Mean","LI","Median","LS")
mfit<-matrix(c(meanest,q2est,sdest,q1est,q3est),p,5)
colnames(mfit)=c("Mean","Median","Sd","Lower","Upper")
rownames(mfit)=paste0(c('\u03b8'),1:p)
if(is.null(LabelParTheta)==FALSE){rownames(mfit)=LabelParTheta}
ngssm.list<-list(mfit,ci*100,nn)
names(ngssm.list)<-c("Bayesian Estimation","Nom. Level(%)","n.obs")
#names(ngssm.list)<-c("\n*****Non-Gaussian State Space Models with Exact Likelihood*****\n",
#"\nNGSSMEL Package:","Bayes - SR",model,"Cred. Level(%)","n.obs")
#cat("\n*****Non-Gaussian State Space Models with Exact Likelihood*****\n",
#"\nNGSSMEL Package:","Bayes - SR",model,"\n") 
if(verbose) print (ngssm.list)
ngssm.list<-list(mfit,ci*100,nn,meanest)
names(ngssm.list)<-c("Bayesian Estimation","Nom. Level(%)","n.obs","coefficients")
ngssm.list=list(ngssm.list,postsample)
#ngssm.list[[1]]
if(postplot==TRUE){
 # dev.new()
message("\nMarginal posterior graphs...")
message("\nTime:....\n")
oldpar <- par(mar=rep(2, 4),mfrow=c(p,2))
on.exit(par(oldpar)) #line i+1
#par(mar=rep(2, 4))
#par(mfrow=c(p,2))
for(iii in 1:p){
if(iii==1){
message("\n","\t","Graph=",iii)
}else{ message("\t",iii)}
xlabbp=paste0(c('\u03b8'),iii)
if(is.null(LabelParTheta)==FALSE){xlabbp=LabelParTheta[iii]}
rb1=aggregate(as.data.frame(dpostvector[,iii]), by=as.data.frame(grid_output[,iii]),FUN=sum)
plot(round(rb1[[1]],3),round(rb1[[2]],3),type='l',xlim=c(linf[iii],lsup[iii]),xlab=xlabbp,ylab="",axes=F,main=xlabbp)
lines(c(meanest[iii],meanest[iii]),c(min(rb1),max(rb1)),lwd=2,col="red") # posterior mean
#lines(c(modeest[iii],modeest[iii]),c(min(rb1),max(rb1)),lwd=2,col="red") # posterior mode
lines(c(q1est[iii],q1est[iii]),c(min(rb1),(max(rb1)-min(rb1))/3),lwd=2,col="blue",lty=c(2)) # linf
lines(c(q3est[iii],q3est[iii]),c(min(rb1),(max(rb1)-min(rb1))/3),lwd=2,col="blue",lty=c(2)) # lsup
axis(1,round(rb1[[1]],4))
legend("topleft", c("Posterior Mean","Cred.Int"),col=c("red","blue"),lty=c(1,2),bty = "n")
}
message("\nDone!\n")
}#end post plot
if(p==1 && contourplot==TRUE){contourplot=FALSE;
warning("\n","It is not possible to build countour plot with p=1!")
}
#,contourplot=FALSE
if(contourplot==TRUE){
message("\nContour posterior graphs...")
message("\nTime:.....\n")
##########################################################
##### Building Lower Triangular Matrix
#p=3
if(p==1){
warning("\n","It is not possible to build countour plot with p=1!")
}else{
numgraphs=choose(p,2)+p
vii=numeric(numgraphs)
vj=numeric(numgraphs)
comp=0
for(jjj in 2:p){
iii=1
if(jjj==2){
message("\n","\t","Graph=",jjj-1)
}else{message("\t",jjj-1)}
while((iii<jjj)&&(iii!=jjj)){
#cat("\n",iii,jjj,comp+1)
vii[comp+1]=iii
vj[comp+1]=jjj
iii=iii+1
comp=comp+1
}
}
}
#vii
#vj
if(p==1){vii=vj=1}
pend=choose(p,2)+1
vii[pend:(numgraphs)]=10000
vj[pend:(numgraphs)]=10000
viij=data.frame(vii,vj)
viij
viijs=viij[order(viij$vii),]
viis=viijs$vii
viis
vjs=viijs$vj
vjs
viis[pend:(numgraphs)]=1:p
vjs[pend:(numgraphs)]=1:p

##########################################################
numgraphs=choose(p,2)+p
mpost=matrix(logpost)
#akima1<-0
#akima<-0
#data(akima)
#assign("akima","new", envir = .GlobalEnv)
#assign(akima,data(akima), envir = .GlobalEnv)
#akima1=akima
#akima1<-0
#akima<-0

##data(akima)
##assign("akima", akima, envir = .GlobalEnv)
##akima1=akima

mpost=matrix(logpost)
#par(mar=rep(1, 4))
#par(mfrow=c(numgraphs,2))
# dev.new()
oldpar <- par(mar=c(5,5,1,1))
on.exit(par(oldpar)) #line i+1
#par(mar=c(5,5,1,1))
#par(mfrow=c(p-1,p-1))
#if(p==2){m=matrix(c(1:((p-1)*(p-1))),p-1,p-1,byrow=T)
#}else{
m=matrix(c(1:((p)*(p))),p,p,byrow=TRUE)
#}
ltm=lower.tri(m)
#if(p==2){
xlay=rep(0,(p-1)*(p-1))
#}else{
xlay=rep(0,p*p)
#}
auxp=m[ltm]
numbergraph=choose(p,2)
auxnumbergraph=1:(numbergraph)

xlay[auxp]=auxnumbergraph

#if(p==2){layout(c(xlay+1))
#}else{
ma=matrix(xlay, p, p, byrow = TRUE)
maa=ma
for(kk in 1:p){maa[kk,kk]=choose(p,2)+kk
#}
layout(maa)
}

for(jjj in 1:numgraphs){

#par(c(vii[jjj],vii[jjj]))
#contour plot
xlabbx=paste0(c('\u03b8'),viis[jjj])
xlabby=paste0(c('\u03b8'),vjs[jjj])
if(is.null(LabelParTheta)==FALSE){xlabbx=LabelParTheta[viis[jjj]]
xlabby=LabelParTheta[vjs[jjj]]
}

if(viis[jjj]!=vjs[jjj]){
#vii[jjj]
#vj[jjj]
#beta2=read.table("resultlikdelta.csv",sep=";",dec=",") 
#w1=read.table("resultlikw.csv",sep=";",dec=",")
#persp(grid_output[,vii[jjj]],grid_output[,vj[jjj]],(dpostvector[,vj[jjj]),theta=45,phi=30,expand=0.7,col = "lightblue",
#ltheta = 120, shade = 0.75, ticktype = "detailed",xlab = "w", ylab = "Beta", zlab = "Post")

fld <- with(list(round(grid_output[,viis[jjj]],3),round(grid_output[,vjs[jjj]],3),log(dpost)),
interp(round(grid_output[,viis[jjj]],3),round(grid_output[,vjs[jjj]],3),log(dpost),duplicate="mean"))
contour(fld,drawlabels = FALSE,col = "black",xlab = xlabbx, ylab = xlabby, method = "edge",nlevels=15)

#abline(h=mean(vii[jjj]), v=mean(vj[jjj]), lwd=1)
#points(c(meanest[vii[jjj]]),c(meanest[vj[jjj]]),col="red",lwd=c(2),pch=16)
#legend("topleft", c("Posterior Mean"),col="red",pch=16,bty = "n")

#legend("topleft", c("Posterior Mode"),col="red",pch=16,bty = "n")
#points(c(modeest[vii[jjj]]),c(modeest[vj[jjj]]),col="red",lwd=c(2),pch=16,bty = "n")

#if(jjj==1){title("Contour Plots")}
#image(fld)
#persp((grid_output[,vii[jjj]]),(grid_output[,vj[jjj]]),logpost,
#drawlabels = FALSE,col = "black",xlab = "w", ylab = "Beta", method = "edge")
}

if(postplot==TRUE){
if(viis[jjj]==vjs[jjj]){
if(viis[jjj]==1){
message("\n","\t","Graph=",viis[jjj])
}else{message("\t",viis[jjj])}
xlabbp=paste0(c('\u03b8'),viis[jjj])
if(is.null(LabelParTheta)==FALSE){xlabbp=LabelParTheta[viis[jjj]]}
rb1=aggregate(as.data.frame(dpostvector[,viis[jjj]]), by=as.data.frame(grid_output[,viis[jjj]]),FUN=sum)
plot(round(rb1[[1]],3),round(rb1[[2]],3),type='l',xlim=c(linf[viis[jjj]],lsup[viis[jjj]]),xlab=xlabbp,ylab="",axes=F,main=xlabbp)
lines(c(meanest[viis[jjj]],meanest[viis[jjj]]),c(min(rb1),max(rb1)),lwd=2,col="red") # posterior mean
#lines(c(modeest[iii],modeest[iii]),c(min(rb1),max(rb1)),lwd=2,col="red") # posterior mode
lines(c(q1est[viis[jjj]],q1est[viis[jjj]]),c(min(rb1),(max(rb1)-min(rb1))/3),lwd=2,col="blue",lty=c(2)) # linf
lines(c(q3est[viis[jjj]],q3est[viis[jjj]]),c(min(rb1),(max(rb1)-min(rb1))/3),lwd=2,col="blue",lty=c(2)) # lsup
axis(1,round(rb1[[1]],4))
legend("topright", c("Posterior Mean","Cred.Int"),col=c("red","blue"),lty=c(1,2),bty = "n")
}
}

}

message("\nDone!\n")

} # end contour plot

message("\nDone!\n")
#} #end post_plot

message("End!")
#return(ngssm.list)
}

if(model=="PEM"){                                   #PEM
#amp=FALSE
#Begin Default StarPar
if(is.null(StaPar)==FALSE){
StaPar1=StaPar;
StaPar1[1]=log(-log(StaPar[1]));
};

if(is.null(Zt)==FALSE)stop("Bad input for Zt for this model")
################################################################################ 
#
## PEM Example 
##
################################################################################
if(verbose) cat ("\n*****Non-Gaussian State Space Models with Exact Likelihood*****\n",
"\nNGSSEML Package:","Bayes -",model,"\n")
#cat("\nTime:.")
message("\nPosterior inputs...")
message("\nDone!")
#cat("\nTime:..")
message("\n")
message("\nPosterior computations...")
message("\n")
set.seed(1000)

#############################################################################################
if(is.null(LabelParTheta)){   #BeginDefaultLabel
if (model=="PEM"){ #Begin Poisson
    if(is.null(Xt)){
          LabelParTheta=c("w")
    }else{
          pp=dim(Xt)[2]
          LabelParThetaaux=c("w","Beta1","Beta2","Beta3","Beta4","Beta5","Beta6","Beta7","Beta8","Beta9","Beta10",
          "Beta11","Beta12","Beta13","Beta14","Beta15","Beta16","Beta17","Beta18","Beta19","Beta20")
          LabelParTheta=LabelParThetaaux[1:(1+pp)]
          }

}
}#EndDefaultLabel
#############################################################################################


#############################
###GRID
#############################

#############################################################################################
if ((is.null(LabelParTheta)==FALSE)){
 if(is.null(StaPar)==FALSE){
    if(length(LabelParTheta)!=length(StaPar))stop("Bad input for LabelParTheta for this model")
 }
}
#if(length(LabelParTheta)!=length(StaPar))stop("Bad input for LabelParTheta for this model")}
# If default of StaPar is true, initialize StaPar.
#Begin Default StarPar
if(is.null(StaPar)==FALSE){
StaPar1=StaPar;
StaPar1[1]=log(-log(StaPar[1]));
};
if(is.null(StaPar)){
if (model=="PEM"){ #Begin PEM
##StaPar:
if(is.null(Xt)){dxt=0}else{dxt=dim(Xt)[2];}
if(is.null(Zt)){dzt=0}else{dzt=0;}
pp=(1+dxt+0);
StaPar=numeric(pp);StaPar1=StaPar;
StaPar[1]=0.8;StaPar1[1]=log(-log(StaPar[1]));if(dxt>0){StaPar[2:(dxt+1)]=rep(0,dxt);};
##lower:
if(is.null(lower)){
lower=numeric(pp);
blbeta=-100;
bubeta=100;
lower[1]=0.001;if(dxt>0){lower[2:(dxt+1)]=rep(blbeta,dxt);};
};
##upper:
if(is.null(upper)){
upper=numeric(pp);
blmu=-100;
bumu=100;
upper[1]=0.999;if(dxt>0){upper[2:(dxt+1)]=rep(bumu,dxt);};
};
} #End PEM

}#End Default StarPar
#print(StaPar)
#print(lower)
#print(upper)
#############################################################################################

nn=length(Yt)
resultsopt=NA
#resultsopt=tryCatch(optim(StaPar1, hessian=TRUE,LikeF2,
#method="BFGS",na.action=na.action,Yt=Yt,Xt=Xt,Zt=Zt,Break=Break,Event=Event,
#amp=amp,a0=a0,b0=b0,model=model,control = control), error = c)
#print(Yt)
#print(Xt)
#print(Zt)
#print(Break)
#print(Event)
#print(StaPar1)
#print(model)
#print(a0)
#print(b0)
resultsopt=tryCatch(optim(StaPar1, hessian=TRUE,LikeF2,
method="BFGS",na.action=na.action,Yt=Yt,Xt=Xt,Zt=Zt,Break=Break,Event=Event,
a0=a0,b0=b0,model=model,
control = list(maxit = 30000, temp = 2000,trace = FALSE,REPORT = 500)), error = c)
#print(resultsopt)
if(is.null(resultsopt$convergence)){stop("Convergence error! Bad inputs! Sorry!")}else{
if(resultsopt$convergence!=0)stop("Convergence error! Bad inputs! Sorry!")}
estopt=resultsopt$par     # Point Estimates:
estopt
Hessianmatrixopt=-resultsopt$hessian   #Hessian Matrix
Hessianmatrixopt
MIFopt=-solve(Hessianmatrixopt)       #MIF

MIFopt[1,1]=MIFopt[1,1]*(((exp(-exp(estopt[1])))*(-exp(estopt[1])))^2)
MIFopt
estopt[1]=exp(-exp(estopt[1]))

qn=3
p=length(StaPar1)
linf=numeric(p)
lsup=numeric(p)
for(jj in 1:p){   # Intervals for Grid
linf[jj]=estopt[jj]-qn*sqrt(MIFopt[jj,jj])
lsup[jj]=estopt[jj]+qn*sqrt(MIFopt[jj,jj])
}
lsup[1]=lsup[1]-0.0000001
if(linf[1]<0){linf[1]=1e-20}
if(linf[1]>0.95){linf[1]=0.80}
if(lsup[1]>1){lsup[1]=0.99999999}

#Compute the Grid:=============================================================
##
## USING THE BUILDGRID_FUNCTION-2018
##
##=============================================================================
Delta=(lsup-linf)/(pointss-1)
Delta
grid_output=gridfunction(pointss,linf,lsup)  #Call the Grid function
Log.Post.rw.pem<-function(StaPar,formula=formula, data=data,na.action=na.action,pz=pz,nBreaks=nBreaks){ 
log.post=-LikeF(StaPar,na.action=na.action,Yt=Yt,Xt=Xt,Zt=Zt,Break=Break,Event=Event,
model=model,a0=a0,b0=b0,amp=amp)+log(PriorF(StaPar,model=model,
prw=prw,prnu=prnu,prchi=prchi,prmu=prmu,prbetamu=prbetamu,prbetasigma=prbetasigma))
return(log.post)
 }      

np=dim(grid_output)[1]
if(is.null(np)){np=dim(t(t(grid_output)))[[1]];
grid_output=t(t(grid_output))}
logpost=numeric(np)
SEQ  <- seq(1,np)
pb   <- txtProgressBar(1, np, style=3)
TIME <- Sys.time()
for (i in 1:np){
#if(i==1){
#cat("\n","\t","Iter.=",i)
#}else{cat("\t",i)}
logpost[i]=Log.Post.rw.pem(grid_output[i,],
formula=formula,data=data,na.action=na.action,pz=pz,nBreaks=nBreaks)
Sys.sleep(0.02)
setTxtProgressBar(pb, i)
}

## ##estender ordem p! =========================================================
dpost=exp(logpost-mean(logpost)) #      standard post. values
#dpost=exp((logpost-mean(logpost))/sd(logpost)) #
#cat(dpost)
constI=sum(prod(Delta)*dpost)
dpostvector=matrix(0,length(dpost),p)
modeest=numeric(p)
meanest=numeric(p)
sdest=numeric(p)
q1est=numeric(p)
q2est=numeric(p)
q3est=numeric(p)
quantilevector=matrix(0,3,p)
postsample=matrix(0,nsamplex,p)

for(ii in 1:p){
if(ii==1){
message("\nSummary.Calculation","\t","=",ii)
}else{ message("\t",ii)}
dpostvector[,ii]=(prod(Delta[-ii]))*(dpost/constI)
rw=aggregate(as.data.frame(dpostvector[,ii]), by=as.data.frame(grid_output[,ii]),
 FUN=sum)
#Mode:
#posw=which.max(rw[[2]])
#modeest[ii]=rw[[1]][posw]
################################################################################
##
##   EXACT MOMENTS USING NUMERICAL INTEGRATION
##
################################################################################
###
### Summaries for Each  Static Parameter:
###
#w:===============================================
#Mean:
meanest[ii]=sum(rw[[1]]*rw[[2]]*(Delta[ii]))
#medw
#Var:
sdest[ii]=sqrt(sum(rw[[2]]*(rw[[1]]^2)*(Delta[ii]))-meanest[ii]^2)
#varw
#Quantiles:
#linfw=medw-1.96*sqrt(varw)
#lsupw=medw+1.96*sqrt(varw)
#linfw
#lsupw
#cc=sum(rw[[2]]*(Delta[ii]))
conf=ci
alpha1=(1-conf)/2
alpha2=(1+conf)/2
cc=(rw[[2]]*(Delta[ii]))/sum(rw[[2]]*(Delta[ii]))
#cc=y*h
#Linf:
csy=cumsum(cc)
csyt1=csy[csy<=alpha1]

#posq1=table(csy<=alpha1)[[2]]
if(length(table(csy<=alpha1))==1){posq1=1+1}
if(length(table(csy<=alpha1))==2){posq1=table(csy<=alpha1)[[2]]}

#cat(grid_output[posq1,ii],csy[posq1])
#q1est[ii]=grid_output[posq1,ii]
#q1est[ii]=rw[[1]][posq1]
if(csy[posq1]>alpha1){
#q1est[ii]=mean(c(rw[[1]][posq1],rw[[1]][posq1-1]))
sw=csy[posq1]+csy[posq1-1]
q1est[ii]=((csy[posq1]/sw)*rw[[1]][posq1]+(csy[posq1-1]/sw)*rw[[1]][posq1-1])
}else{
#q1est[ii]=mean(c(rw[[1]][posq1],rw[[1]][posq1+1]))
sw=csy[posq1]+csy[posq1+1]
q1est[ii]=((csy[posq1]/sw)*rw[[1]][posq1]+(csy[posq1+1]/sw)*rw[[1]][posq1+1])
}
#Median:
csyt2=csy[csy<=0.50]

#posq2=table(csy<=0.50)[[2]]
if(length(table(csy<=0.5))==1){posq2=1+1}
if(length(table(csy<=0.5))==2){posq2=table(csy<=0.5)[[2]]}

#cat(grid_output[posq2,ii],csy[posq2])
#q2est[ii]=grid_output[posq2,ii]
#q2est[ii]=rw[[1]][posq2]
if(csy[posq2]>0.5){
#q2est[ii]=mean(c(rw[[1]][posq2],rw[[1]][posq2-1]))
sw=csy[posq2]+csy[posq2-1]
q2est[ii]=((csy[posq2]/sw)*rw[[1]][posq2]+(csy[posq2-1]/sw)*rw[[1]][posq2-1])
}else{
#q2est[ii]=mean(c(rw[[1]][posq2],rw[[1]][posq2+1]))
sw=csy[posq2]+csy[posq2+1]
q2est[ii]=((csy[posq2]/sw)*rw[[1]][posq2]+(csy[posq2+1]/sw)*rw[[1]][posq2+1])
}
#Lsup:
csyt3=csy[csy<=alpha2]

#posq3=table(csy<=alpha2)[[2]]
if(length(table(csy<=alpha2))==1){posq3=1+1}
if(length(table(csy<=alpha2))==2){posq3=table(csy<=alpha2)[[2]]}

#q3est[ii]=grid_output[posq3,ii]
#q3est[ii]=rw[[1]][posq3]
if(csy[posq3]>alpha2){
#q3est[ii]=mean(c(rw[[1]][posq3],rw[[1]][posq3-1]))
sw=csy[posq3]+csy[posq3-1]
q3est[ii]=((csy[posq3]/sw)*rw[[1]][posq2]+(csy[posq3-1]/sw)*rw[[1]][posq3-1])
}else{
#q3est[ii]=mean(c(rw[[1]][posq3],rw[[1]][posq3+1]))
sw=csy[posq3]+csy[posq3+1]
q3est[ii]=((csy[posq3]/sw)*rw[[1]][posq3]+(csy[posq3+1]/sw)*rw[[1]][posq3+1])
}
#cat(grid_output[posq3,ii],csy[posq3])
#Mode:
posw=which.max((rw[[2]]))
modeest[ii]=rw[[1]][posw]
################################################################################

## Quantiles and Mean calculate using a sample of marginal posterior distribution:
xw <- sample(rw[[1]],nsamplex,replace=TRUE,prob=rw[[2]]) 
postsample[,ii]=xw
#cat("\nPosterior computations...")
message("\nDone!")
message("\nTime:...\n")
#cat("\nSummaries:", summary(xw))
#meanest[ii]=mean(xw)
#alpha=1-ci
#cat("\nQuantiles:",quantile(xw,probs=c(alpha/2,0.5,1-alpha/2)))
#quantilevector[,ii]=quantile(xw,probs=c(alpha/2,0.5,1-alpha/2))

}

#mfit<-matrix(c(modeest,meanest,t(quantilevector[1,]),t(quantilevector[2,]),t(quantilevector[3,])),p,5)
#colnames(mfit)=c("Mode","Mean","LI","Median","LS")
mfit<-matrix(c(meanest,q2est,sdest,q1est,q3est),p,5)
colnames(mfit)=c("Mean","Median","Sd","Lower","Upper")
rownames(mfit)=paste0(c('\u03b8'),1:p)
if(is.null(LabelParTheta)==FALSE){rownames(mfit)=LabelParTheta}
ngssm.list<-list(mfit,ci*100,nn)
names(ngssm.list)<-c("Bayesian Estimation","Nom. Level(%)","n.obs")
#cat("\n*****Non-Gaussian State Space Models with Exact Likelihood*****\n",
#"\nNGSSMEL Package:","Bayes - Reliability",model,"\n") 
if(verbose) print (ngssm.list)
ngssm.list<-list(mfit,ci*100,nn,meanest)
names(ngssm.list)<-c("Bayesian Estimation","Nom. Level(%)","n.obs","coefficients")

ngssm.list=list(ngssm.list,postsample)
if(postplot==TRUE){
# dev.new()
message("\nMarginal posterior graphs...")
message("\nTime:....\n")
oldpar <- par(mar=rep(2, 4),mfrow=c(p,2))
on.exit(par(oldpar)) #line i+1
#par(mar=rep(2, 4))
#par(mfrow=c(p,2))
for(iii in 1:p){
if(iii==1){
message("\n","\t","Graph=",iii)
}else{ message("\t",iii)}
xlabbp=paste0(c('\u03b8'),iii)
if(is.null(LabelParTheta)==FALSE){xlabbp=LabelParTheta[iii]}
rb1=aggregate(as.data.frame(dpostvector[,iii]), by=as.data.frame(grid_output[,iii]),FUN=sum)
plot(rb1[[1]],rb1[[2]],type='l',xlim=c(linf[iii],lsup[iii]),xlab=xlabbp,ylab="",axes=F,main=xlabbp)
lines(c(meanest[iii],meanest[iii]),c(min(rb1),max(rb1)),lwd=2,col="red") # posterior mean
#lines(c(modeest[iii],modeest[iii]),c(min(rb1),max(rb1)),lwd=2,col="red") # posterior mode
lines(c(q1est[iii],q1est[iii]),c(min(rb1),(max(rb1)-min(rb1))/3),lwd=2,col="blue",lty=c(2)) # linf
lines(c(q3est[iii],q3est[iii]),c(min(rb1),(max(rb1)-min(rb1))/3),lwd=2,col="blue",lty=c(2)) # lsup
axis(1,round(rb1[[1]],3))
legend("topleft", c("Posterior Mean","Cred.Int"),col=c("red","blue"),lty=c(1,2),bty = "n")
}
message("\nDone!\n")
}#end post plot

#,contourplot=FALSE
if(p==1 && contourplot==TRUE){contourplot=FALSE;
warning("\n","It is not possible to build countour plot with p=1!")
}
if(contourplot==TRUE){
message("\nContour posterior graphs...")
message("\nTime:.....\n")
##########################################################
##### Building Lower Triangular Matrix
#p=3
if(p==1){
warning("\n","It is not possible to build countour plot with p=1!")
}else{
numgraphs=choose(p,2)+p
vii=numeric(numgraphs)
vj=numeric(numgraphs)
comp=0
for(jjj in 2:p){
iii=1
if(jjj==2){
message("\n","\t","Graph=",jjj-1)
}else{message("\t",jjj-1)}
while((iii<jjj)&&(iii!=jjj)){
#cat("\n",iii,jjj,comp+1)
vii[comp+1]=iii
vj[comp+1]=jjj
iii=iii+1
comp=comp+1
}
}
}
if(p==1){vii=vj=1}
#vii
#vj
pend=choose(p,2)+1
vii[pend:(numgraphs)]=10000
vj[pend:(numgraphs)]=10000
viij=data.frame(vii,vj)
viij
viijs=viij[order(viij$vii),]
viis=viijs$vii
viis
vjs=viijs$vj
vjs
viis[pend:(numgraphs)]=1:p
vjs[pend:(numgraphs)]=1:p

##########################################################
numgraphs=choose(p,2)+p
mpost=matrix(logpost)
#akima1<-0
#akima<-0
#data(akima)
#assign("akima","new", envir = .GlobalEnv)
#assign(akima,data(akima), envir = .GlobalEnv)
#akima1=akima
#akima1<-0
#akima<-0

##data(akima)
##assign("akima", akima, envir = .GlobalEnv)
##akima1=akima

mpost=matrix(logpost)
#par(mar=rep(1, 4))
#par(mfrow=c(numgraphs,2))
#dev.new()
oldpar <- par(mar=c(5,5,1,1))
on.exit(par(oldpar)) #line i+1

#par(mar=c(5,5,1,1))
#par(mfrow=c(p-1,p-1))
#if(p==2){m=matrix(c(1:((p-1)*(p-1))),p-1,p-1,byrow=TRUE)
#}else{
m=matrix(c(1:((p)*(p))),p,p,byrow=TRUE)
#}
ltm=lower.tri(m)
#if(p==2){
xlay=rep(0,(p-1)*(p-1))
#}else{
xlay=rep(0,p*p)
#}
auxp=m[ltm]
numbergraph=choose(p,2)
auxnumbergraph=1:(numbergraph)

xlay[auxp]=auxnumbergraph

#if(p==2){layout(c(xlay+1))
#}else{
ma=matrix(xlay, p, p, byrow = TRUE)
maa=ma
for(kk in 1:p){maa[kk,kk]=choose(p,2)+kk
#}
layout(maa)
}

for(jjj in 1:numgraphs){

#par(c(vii[jjj],vii[jjj]))
#contour plot
xlabbx=paste0(c('\u03b8'),viis[jjj])
xlabby=paste0(c('\u03b8'),vjs[jjj])
if(is.null(LabelParTheta)==FALSE){xlabbx=LabelParTheta[viis[jjj]]
xlabby=LabelParTheta[vjs[jjj]]
}

if(viis[jjj]!=vjs[jjj]){
#vii[jjj]
#vj[jjj]
#beta2=read.table("resultlikdelta.csv",sep=";",dec=",") 
#w1=read.table("resultlikw.csv",sep=";",dec=",")
#persp(grid_output[,vii[jjj]],grid_output[,vj[jjj]],(dpostvector[,vj[jjj]),theta=45,phi=30,expand=0.7,col = "lightblue",
#ltheta = 120, shade = 0.75, ticktype = "detailed",xlab = "w", ylab = "Beta", zlab = "Post")

fld <- with(list(round(grid_output[,viis[jjj]],3),round(grid_output[,vjs[jjj]],3),log(dpost)),
interp(round(grid_output[,viis[jjj]],3),round(grid_output[,vjs[jjj]],3),log(dpost),duplicate="mean"))

contour(fld,drawlabels = FALSE,col = "black",xlab = xlabbx, ylab = xlabby, method = "edge",nlevels=15)
#abline(h=mean(vii[jjj]), v=mean(vj[jjj]), lwd=1)
#points(c(meanest[vii[jjj]]),c(meanest[vj[jjj]]),col="red",lwd=c(2),pch=16)
#legend("topleft", c("Posterior Mean"),col="red",pch=16,bty = "n")

#legend("topleft", c("Posterior Mode"),col="red",pch=16,bty = "n")
#points(c(modeest[vii[jjj]]),c(modeest[vj[jjj]]),col="red",lwd=c(2),pch=16,bty = "n")

#if(jjj==1){title("Contour Plots")}
#image(fld)
#persp((grid_output[,vii[jjj]]),(grid_output[,vj[jjj]]),logpost,
#drawlabels = FALSE,col = "black",xlab = "w", ylab = "Beta", method = "edge")
}
if(postplot==TRUE){
if(viis[jjj]==vjs[jjj]){
if(viis[jjj]==1){
message("\n","\t","Graph=",viis[jjj])
}else{message("\t",viis[jjj])}
xlabbp=paste0(c('\u03b8'),viis[jjj])
if(is.null(LabelParTheta)==FALSE){xlabbp=LabelParTheta[viis[jjj]]}
rb1=aggregate(as.data.frame(dpostvector[,viis[jjj]]), by=as.data.frame(grid_output[,viis[jjj]]),FUN=sum)
plot(round(rb1[[1]],3),round(rb1[[2]],3),type='l',xlim=c(linf[viis[jjj]],lsup[viis[jjj]]),xlab=xlabbp,ylab="",axes=F,main=xlabbp)
lines(c(round(meanest[viis[jjj]],3),round(meanest[viis[jjj]],4)),c(min(round(rb1,3)),max(round(rb1,3))),lwd=2,col="red") # posterior mean
#lines(c(modeest[iii],modeest[iii]),c(min(rb1),max(rb1)),lwd=2,col="red") # posterior mode
lines(c(q1est[viis[jjj]],q1est[viis[jjj]]),c(min(rb1),(max(rb1)-min(rb1))/3),lwd=2,col="blue",lty=c(2)) # linf
lines(c(round(q3est[viis[jjj]],3),round(q3est[viis[jjj]],3)),c(min(round(rb1,3)),(max(round(rb1,3))-min(round(rb1,3)))/3),lwd=2,col="blue",lty=c(2)) # lsup
axis(1,round(rb1[[1]],4))
legend("topright", c("Posterior Mean","Cred.Int"),col=c("red","blue"),lty=c(1,2),bty = "n")
}
}

} 
message("\nDone!\n")


#if(postplot==TRUE){
# dev.new()
#cat("\nMarginal posterior graphs...")
#cat("\nTime:....\n")
##par(mar=rep(2, 4))
##par(mfrow=c(p,2))
#for(iii in 1:p){

#}

message("\nDone!\n")
#} #end post_plot


} # end contour plot


message("End!")
#print(ngssm.list[[2]])
#return(ngssm.list)
}
#return(ngssm.list)
  obj <- list()
  #fitfit<-list(model, formula,meanest)
  fit<-list(sys = sys.call(),"Mean.Post",meanest)
  names(fit)[3]=list("coefficients")
  obj$fit <-fit
  obj$model<-model
  obj$formula<-formula
  obj$pz<-pz
  obj$na.action<-na.action
  obj$a0<-a0
  obj$b0<-b0
  obj$nBreaks<-nBreaks
  obj$ci<-ci
  if(aans){method="ARMS"}else{method="Numerical Integration"}
  obj$method<-list("Bayesian Estimation",method)
  obj$nBreaks<-nBreaks
  obj$pointss<-pointss
  obj$nsample<-nsamplex
  obj$cov <- cov(ngssm.list[[2]])
  estoptc<-mfit[,1]
  if(is.null(LabelParTheta)){names(estoptc)=paste0(c('\u03b8'),1:p)}else{names(estoptc)=LabelParTheta}
  obj$coefficients<-list("Mean.Post",estoptc)
  obj$data<-data
  MeanSmooth=SmoothingF(formula=formula,data=data,model=model,
                        a0=a0,b0=b0,pz=pz,Type="Marg",amp=amp,samples=1,nBreaks=nBreaks,ci=ci,splot=FALSE,StaPar=ngssm.list[[2]])
  aaff<-MeanSmooth[[1]][,1]
  nnpp<-nn
  #print(nnpp)
  if(model=="PEM"){nnpp<-length(Break)-1}
  names(aaff)<-1:nnpp
  obj$fitted.values<-list("Smoothed estimates",aaff) # FilteringF function
  obj$y<-obj$fitted.values
  #names(obj$y)<-c("Smoothed estimates")
  pnn<-length(obj$fitted.values)
  obj$x<-1:pnn
  #names(obj$x)<-c("Order obs.")
 # obj$summary<-list(cat ("\n*****Non-Gaussian State Space Models with Exact Likelihood*****\n","\nNGSSEML Package:","Bayes -",model,"\n"),ngssm.list[[1]]) #colocar a lista que criei de output
   obj$summary<-list("*****Non-Gaussian State Space Models with Exact Likelihood*****\nNGSSEML Package:Bayes -",model,ngssm.list[[1]]) #colocar a lista que criei de output
  
  obj$samplepost<-ngssm.list[[2]]
  #obj$data<-data
  class(obj) = "ngssm.bayes"
  ob<-ngssm.list[[1]]; 
  class(ob) = "ngssm.bayes"
  if(verbose==TRUE) {obj<-obj}else{obj<-ob}
  return(obj) 
  
    
}#End Numerical Integration
################################################################################

}#End ngssm.Bayes
################################################################################
