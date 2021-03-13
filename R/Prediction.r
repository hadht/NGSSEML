################################################################################
##
## Prediction One-Step-Ahead Function for Observations
##
################################################################################
##
##
#'@noRd
Prediction<- function(formula,data,na.action="na.omit",pz=NULL,nBreaks=NULL,
                        model="Poisson",StaPar=NULL,a0=0.01,b0=0.01,distl="PRED",
                        ci=0.95,samples=500,hh=1,Xtprev=NULL,method="MLE"){

if (method!="MLE" && method!="Bayes")stop("Bad input for method")

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
    Break=GridP(Y, Event, nT = nBreaks)
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
  }else{
    Event=NULL
    Break=NULL
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
 # print(Yt)
#  print(Xt)
#  print(Zt)
#  print(Event)
#  print(Break)

  ###################################################################################
  ###################################################################################
  ###################################################################################


# DataFrame:
#dataf<-data
#dataf<-dataf[all.vars(formula)]
#Dataframe data
#if(length(all.vars(formula))> dim(data)[2])stop("Check the formula and data.")
#if(is.data.frame(data)==FALSE)stop("The argument needs to be a data frame.")

#attach(dataf)

#if(model=="PEM"){
##Event=get(names(dataf)[2])
#dataf<-data
#dataf<-dataf[c(all.vars(formula)[1],colnames(data)[2],all.vars(formula)[-1])]
#Event<-NULL
#Break<-NULL

##Dataframe data
#if(length(all.vars(formula))> dim(data)[2])stop("Check the formula and data.")
#if(is.data.frame(data)==FALSE)stop("The argument needs to be a data frame.")

##dataf<-dataf[all.vars(formula)]
##Yt=get(names(dataf)[1])
#Ytdd=dataf[[colnames(dataf)[1]]]
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
  ##########################################################

if(model!="PEM"){
dataf<-data
dataf<-dataf[all.vars(formula)]
#Dataframe data
if(length(all.vars(formula))> dim(data)[2])stop("Check the formula and data.")
if(is.data.frame(data)==FALSE)stop("The argument needs to be a data frame.")
Ytdd=dataf[[colnames(dataf)[1]]]
Xtdd=NULL
Ztdd=NULL
if(is.null(pz)){
if(dim(dataf)[2]>1){
nnnd=dim(dataf)[1]
ppd=dim(dataf)[2]-1
Xtdd=matrix(0,nnnd,ppd)
for(i in 1:ppd){
#Xt[,i]=get(names(dataf)[i+1])
##print(get(names(dataf)[i+1]))
Xtdd[,i]=dataf[[names(dataf)[i+1]]]

}
}
}
 if(is.null(pz)!=TRUE){
nnnd=dim(dataf)[1]
ppd=dim(dataf)[2]-1-pz
Xtdd=matrix(0,nnnd,ppd)
if(pz>=1){
for(i in 1:ppd){
#Xt[,i]=get(names(dataf)[i+1])
Xtdd[,i]=dataf[[names(dataf)[i+1]]]
}
}
Zt=matrix(0,nnnd,pz)
for(j in 1:pz){
#Zt[,j]=get(names(dataf)[j+ppd+1])
Ztdd[,j]=dataf[[names(dataf)[j+ppd+1]]]

}
}
}
Yt<-Ytdd
Xt<-Xtdd
Zt<-Ztdd
#detach(dataf)
#print(Yt)
#print(Xt)
#print(Zt)


if(method=="MLE"){ #Begin MLE
 if (a0 <= 0) stop("Bad input value for a0")
     if (b0 <= 0) stop("Bad input value for b0")
     if (is.null(Yt))stop("Bad input Yt")
     if (is.vector(Yt)==FALSE)stop("Bad input for Yt")
     if (distl!="PRED" && distl!="FILTER")stop("Bad input for distl")
     if (is.vector(Xt))stop("Bad input for Xt. Put as a matrix.")
     if (is.null(StaPar))stop("Bad input for StaPar")
     if (is.data.frame(StaPar))stop("Bad input for StaPar")
     if (is.vector(StaPar)==FALSE)stop("Bad input for StaPar")
     if (model!="SRWeibull"&&model!="SRGamma"&&model!="Poisson")stop("Bad input for model")
     if (sum(length(which(is.na(Yt))))>0)stop("Bad input Yt")
     if (ci<=0 | ci>=1)stop("Bad input for ci")
     if(is.null(Xt)==FALSE){if (sum(length(which(is.na(Xt))))>0)stop("Bad input Xt")}
     if(is.null(Xt)==FALSE){if(is.matrix(Xt)==FALSE){Xt=as.matrix(Xt)}}
     if(is.null(Zt)==FALSE){if(is.matrix(Zt)==FALSE){Zt=as.matrix(Xt)}}
     if(StaPar[1]==0)stop("Bad input for the static parameter w: value outside the parameter space.")
     alpha=1-ci
     n<-length(Yt)
     if(n==1){n<-length(t(Yt))}
     Mean<-numeric(hh)
     Median<-numeric(hh)
     Perc1<-numeric(hh)
     Perc2<-numeric(hh)
     mu<-numeric(samples)
     if(is.null(Xt)==FALSE){
       if(is.null(dim(Xt))){
        dbeta=dim(t(Xt))[1]
        dStaPar=length(StaPar)
        Beta=matrix(StaPar[(dStaPar-dbeta+1):(dStaPar)],dbeta,1)
        }else{
        dbeta=dim(Xt)[2]
        dStaPar=length(StaPar)
        Beta=matrix(StaPar[(dStaPar-dbeta+1):(dStaPar)],dbeta,1)
        }
      }
     if(model=="SRWeibull"){
     filpar=FilteringF(StaPar=StaPar,formula=formula,data=data,model="SRWeibull",pz=pz,nBreaks=nBreaks,a0,b0,amp=FALSE,distl="FILTER",splot=FALSE)#weibull
    # print(filpar)
     }
     if(model=="SRGamma"){
  #   print("OK!")
   #  print(formula)
    # print(data)
     #print(StaPar)
    # print(FilteringF(StaPar=StaPar,formula=formula,data=data,model="SRGamma",pz=NULL,a0,b0,distl="FILTER"),splot=FALSE)
     filpar=FilteringF(StaPar=StaPar,formula=formula,data=data,model="SRGamma",pz=pz,nBreaks=nBreaks,a0,b0,distl="FILTER",splot=FALSE)  #gamma
     #print(filpar)
     #print("OK!")
     }
     if(model=="Poisson"){
     filpar=FilteringF(formula=formula,na.action=na.action,data=data,model="Poisson",pz=pz,nBreaks=nBreaks,StaPar=StaPar,a0=a0,b0=b0,amp=FALSE,distl="FILTER",splot=FALSE)  #gamma
     #print(filpar)
     }
     att=filpar[1,]
     btt=filpar[2,]
     ahh<-matrix(0,samples,hh+1)
     bhh<-matrix(0,samples,hh+1)
     muhh<-matrix(0,samples,hh)
     lambda<-matrix(0,samples,hh)
     ahh[,1]<-att[n]
     bhh[,1]<-btt[n]
     for(tt in 1:hh){
     for(ii in 1:samples){
         lambda[ii,tt] <- rgamma(1,StaPar[1]*ahh[ii,tt],rate=StaPar[1]*bhh[ii,tt])+1.0e-20
            if(model=="SRWeibull"){
             muhh[ii,tt]=rweibull(1,StaPar[2],(lambda[ii,tt]*exp(-StaPar[2]*Xtprev[tt,1:dbeta]%*%Beta))^(-1/StaPar[2])) #weibull
             ahh[ii,tt+1]<-StaPar[1]*ahh[tt]+1
             bhh[ii,tt+1]<-StaPar[1]*bhh[tt]+(muhh[ii,tt]^(StaPar[2]))*1
                }
            if(model=="SRGamma"){
             muhh[ii,tt]=rgamma(1,StaPar[2],rate=lambda[ii,tt]*exp(-Xtprev[tt,1:dbeta]%*%Beta))     #gamma
             ahh[ii,tt+1]<-StaPar[1]*ahh[tt]+StaPar[2]
             bhh[ii,tt+1]<-StaPar[1]*bhh[tt]+(muhh[ii,tt])*1
            }

            if(model=="Poisson"){
             muhh[ii,tt]=rpois(1,lambda[ii,tt]*exp(-Xtprev[tt,1:dbeta]%*%Beta))     #gamma
             ahh[ii,tt+1]<-StaPar[1]*ahh[tt]+muhh[ii,tt]
             bhh[ii,tt+1]<-StaPar[1]*bhh[tt]+1
            }
         }
         Mean[tt]=mean(muhh[,tt],na.rm=TRUE) #mean
         Median[tt]=median(muhh[,tt],na.rm=TRUE) #median
         Perc1[tt]=quantile(muhh[,tt],probs=c(alpha/2),na.rm=TRUE)    #perc alpha/2
         Perc2[tt]=quantile(muhh[,tt],probs=c(1-(alpha/2)),na.rm=TRUE)   #perc 1-alpha/2

        }
 return(data.frame(Mean,Median,Perc1,Perc2))
 }#End MLE

### Bayesian Method: ##########################################################
if(method=="Bayes"){ #Begin Bayes
     if (a0 <= 0) stop("Bad input value for a0")
     if (b0 <= 0) stop("Bad input value for b0")
     if (is.null(Yt))stop("Bad input Yt")
     if (is.vector(Yt)==FALSE)stop("Bad input for Yt")
     if (distl!="PRED" && distl!="FILTER")stop("Bad input for distl")
     if (is.vector(Xt))stop("Bad input for Xt. Put as a matrix.")
     if (is.null(StaPar))stop("Bad input for StaPar")
     if (is.matrix(StaPar)==FALSE)stop("Bad input for StaPar")
     if (model!="SRWeibull"&&model!="SRGamma"&&model!="Poisson")stop("Bad input for model")
     if (sum(length(which(is.na(Yt))))>0)stop("Bad input Yt")
     if (ci<=0 | ci>=1)stop("Bad input for ci")
     if(is.null(Xt)==FALSE){if (sum(length(which(is.na(Xt))))>0)stop("Bad input Xt")}
     nsamplex=length(StaPar[,1])
     if(table(StaPar[,1]==0)[[1]]!=nsamplex)stop("Bad input for the static parameter w: value outside the parameter space.")
     alpha=1-ci
     n<-length(Yt)
     if(n==1){n<-length(t(Yt))}
     Mean<-numeric(hh)
     Median<-numeric(hh)
     Perc1<-numeric(hh)
     Perc2<-numeric(hh)
     mu<-numeric(samples)
     set.seed(1000)
     MeanSmoothme=matrix(0,hh,nsamplex)
     MeanSmoothmed=matrix(0,hh,nsamplex)
     MeanSmoothperc1=matrix(0,hh,nsamplex)
     MeanSmoothperc2=matrix(0,hh,nsamplex)
     filpar1=matrix(0,n,nsamplex)
   for(j in 1:nsamplex){ #for Bayes
     if(is.null(Xt)==FALSE){
       if(is.null(dim(Xt))){
        dbeta=dim(t(Xt))[1]
        dStaPar=length(StaPar[j,])
        Beta=matrix(StaPar[j,(dStaPar-dbeta+1):(dStaPar)],dbeta,1)
        }else{
        dbeta=dim(Xt)[2]
        dStaPar=length(StaPar[j,])
        Beta=matrix(StaPar[j,(dStaPar-dbeta+1):(dStaPar)],dbeta,1)
        }
      }
     if(model=="SRWeibull"){
     filpar=FilteringF(StaPar=StaPar[j,],formula=formula,data=data,model="SRWeibull",pz=pz,nBreaks=nBreaks,a0,b0,amp=FALSE,distl="FILTER",splot=FALSE)#weibull
     }
     if(model=="SRGamma"){
     filpar=FilteringF(StaPar=StaPar[j,],formula=formula,data=data,model="SRGamma",pz=pz,nBreaks=nBreaks,a0,b0,amp=FALSE,distl="FILTER",splot=FALSE)  #gamma
     }
     if(model=="Poisson"){
     filpar=FilteringF(StaPar=StaPar[j,],formula=formula,data=data,model="Poisson",pz=pz,nBreaks=nBreaks,a0,b0,amp=FALSE,distl="FILTER",splot=FALSE)  #gamma
     }
     att=filpar[1,]
     btt=filpar[2,]
     ahh<-matrix(0,samples,hh+1)
     bhh<-matrix(0,samples,hh+1)
     muhh<-matrix(0,samples,hh)
     lambda<-matrix(0,samples,hh)
     ahh[,1]<-att[n]
     bhh[,1]<-btt[n]
     for(tt in 1:hh){
     for(ii in 1:samples){
         lambda[ii,tt] <- rgamma(1,StaPar[j,1]*ahh[ii,tt],rate=StaPar[j,1]*bhh[ii,tt])+1.0e-20
            if(model=="SRWeibull"){
             muhh[ii,tt]=rweibull(1,StaPar[j,2],(lambda[ii,tt]*exp(-StaPar[j,2]*Xtprev[tt,1:dbeta]%*%Beta))^(-1/StaPar[j,2])) #weibull
             ahh[ii,tt+1]<-StaPar[j,1]*ahh[tt]+1
             bhh[ii,tt+1]<-StaPar[j,1]*bhh[tt]+(muhh[ii,tt]^(StaPar[j,2]))*(exp(-StaPar[j,2]*Xtprev[tt,1:dbeta]%*%Beta))
                }
            if(model=="SRGamma"){
             muhh[ii,tt]=rgamma(1,StaPar[j,2],rate=lambda[ii,tt]*exp(-Xtprev[tt,1:dbeta]%*%Beta))     #gamma
             ahh[ii,tt+1]<-StaPar[j,1]*ahh[tt]+StaPar[j,2]
             bhh[ii,tt+1]<-StaPar[j,1]*bhh[tt]+(muhh[ii,tt])*exp(-Xtprev[tt,1:dbeta]%*%Beta)
            }
            if(model=="Poisson"){
             muhh[ii,tt]=rpois(1,lambda[ii,tt]*exp(Xtprev[tt,1:dbeta]%*%Beta))     #gamma
             ahh[ii,tt+1]<-StaPar[j,1]*ahh[tt]+muhh[ii,tt]
             bhh[ii,tt+1]<-StaPar[j,1]*bhh[tt]+exp(Xtprev[tt,1:dbeta]%*%Beta)
            }
         }
         Mean[tt]=mean(muhh[,tt],na.rm=TRUE) #mean
         Median[tt]=median(muhh[,tt],na.rm=TRUE) #median
         Perc1[tt]=quantile(muhh[,tt],probs=c(alpha/2),na.rm=TRUE)    #perc alpha/2
         Perc2[tt]=quantile(muhh[,tt],probs=c(1-(alpha/2)),na.rm=TRUE)   #perc 1-alpha/2
        }
     MeanSmoothme[,j]=Mean
     MeanSmoothmed[,j]=Median
     MeanSmoothperc1[,j]=Perc1
     MeanSmoothperc2[,j]=Perc2
} # end for Bayes

Meanbayes=apply(MeanSmoothme,1,mean)
Medianbayes=apply(MeanSmoothmed,1,mean)
Q1bayes=apply(MeanSmoothperc1,1,mean)
Q2bayes=apply(MeanSmoothperc2,1,mean)
return(data.frame(Meanbayes,Medianbayes,Q1bayes,Q2bayes))
 }#End Bayes

} #end function

##########################################################


