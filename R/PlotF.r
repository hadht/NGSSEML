

##########################################################################################
##
##   PLOT FUNCTION VERSION 2
##
##########################################################################################
##
##
#'@export
PlotF<-function(formula, data,na.action="na.omit",pz=NULL,nBreaks=NULL,
plotYt=TRUE,axisxdate=NULL,transf=1,model="Poisson",posts,Proc="Smooth",Type="Marg",
distl="PRED",a0=0.01,b0=0.01,ci=0.95,startdate=NULL,enddate=NULL,Freq=NULL,...){
#argumentsdesignplot <- list(...)
  arg <- list(...)
## ... = typeline='l', arg1
#cols="black", arg2
 #      "blue",
#       "lightgrey"),
#xxlab="t", arg3
#  yylab=expression(paste(hat(mu)[t])), arg4
#,xxlim=NULL, arg5
#yylim=NULL, arg6
#Lty=c(1,2,1), arg7
##  Lwd=c(2,2,2), arg8
#Cex=0.68, arg9


#argument breaks ==FALSE or TRUE

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
#  Event=NULL
  Break=NULL
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
    Break=GridP(Y, Event, nT = nBreaks)
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
 # cat("Yt=",Yt)
 # print(Xt)
#  print(Zt)
#  print(Event)
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
###################################################################################



if(is.null(Xt)==FALSE){if(is.matrix(Xt)==FALSE){Xt=as.matrix(Xt)}}
if(is.null(Zt)==FALSE){if(is.matrix(Zt)==FALSE){Zt=as.matrix(Xt)}}

if(Proc=="Smooth"){
#Smoothing:
set.seed(1000)
nn=length(Yt)
samples=1
if(is.null(dim(posts))){samples=2000}
if(model=="PEM"){nn=length(Break)-1}
fits=SmoothingF(StaPar=posts,formula=formula,data=data,pz=pz,model=model,Type=Type,
a0=a0,b0=b0,ci=ci,splot=FALSE,samples=samples)

## Graph:
ytm=matrix(0,nn,4)
if(model=="PEM"){}else{ytm[,1]=Yt}
alpha=1-ci
#aplicar uma transf.
sums=fits[[1]]
if(is.null(dim(posts))){sums=fits}
ytm[,1]=ytm[,1]^(transf)
ytm[,2]=sums[,1]^(transf)
ytm[,3]=sums[,3]^(transf)
ytm[,4]=sums[,4]^(transf)
minyt=min(ytm)
maxyt=max(ytm)
if(is.null(axisxdate)){
#Seq. ordem das obs.
at=1:nn
}else{
#Date
#Deixar especificar o eixo date at!
#at = seq(as.Date(startdate),as.Date(enddate),Freq)
at=axisxdate
}
d=at
#if(is.null(startdate)==FALSE & is.null(enddate)==FALSE){
#dat=seq(d[1], d[length(d)], by="month")}
if(is.null(arg[[6]])|is.null(arg[[6]])){
plot(at,ytm[,2],xlab=arg[[3]],ylab=arg[[4]],type=arg[[1]],axes=FALSE,ylim=c(minyt,maxyt),col="white")
seq1=at
seq2=sort(at,decreasing = TRUE)
xxx=sort(at,decreasing = TRUE)
polygon(c(at, xxx),c((ytm[,4]),rev((ytm[,3]))),ylim=c(minyt,maxyt),col=arg[[2]][3],border=arg[[2]][3])
if(plotYt==TRUE){
par(new=TRUE)
plot(at,ytm[,1],xlab=arg[[3]],ylab=arg[[4]],type=arg[[1]],axes=TRUE,ylim=c(minyt,maxyt),lty=arg[[7]][1],
lwd=arg[[8]][1],col=arg[[2]][1])
}
par(new=TRUE)
plot(at,ytm[,2],xlab=arg[[3]],ylab=arg[[4]],type=arg[[1]],axes=TRUE,ylim=c(minyt,maxyt),lty=arg[[7]][2],
lwd=arg[[8]][2],col=arg[[2]][2],cex=arg[[9]])
}else{
plot(at,ytm[,2],xlab=arg[[3]],ylab=arg[[4]],type=arg[[1]],axes=FALSE,ylim=arg[[6]],col="white",cex=arg[[9]])
seq1=at
seq2=sort(at,decreasing = TRUE)
xxx=sort(at,decreasing = TRUE)
polygon(c(at, xxx),c((ytm[,4]),rev((ytm[,3]))),ylim=arg[[6]],col=arg[[2]][3],border=arg[[2]][3])
if(plotYt==TRUE){
par(new=TRUE)
plot(at,ytm[,1],xlab=arg[[3]],ylab=arg[[4]],type=arg[[1]],ylim=arg[[6]],axes=TRUE,lty=arg[[7]][1],
lwd=arg[[8]][1],col=arg[[2]][1],cex=arg[[9]])
}
par(new=TRUE)
plot(at,ytm[,2],xlab=arg[[3]],ylab=arg[[4]],type=arg[[1]],ylim=arg[[6]],axes=TRUE,lty=arg[[7]][2],
lwd=arg[[8]][2],col=arg[[2]][2],cex=arg[[9]])

}
#axis.Date(1,at=dat,labels=dat, las = 1)
##axis(2, at =round(seq(min(ytm[,4]), max(ytm[,4]),((max(ytm[,4])-min(ytm[,4]))/10)),digits=2), las = 1, tck = +0.01,cex.axis=0.7)
##if(is.null(startdate) | is.null(enddate)|is.null(Freq)|is.null(axisxdate)){
#Seq. ordem das obs.
#seq(min(ytm[,4]), max(ytm[,4]),((max(ytm[,4]))/10))
#axis(1, at =at , las = 1, tck =+0.01,cex.axis=0.5)
##axis(1, at =round(seq(min(att), max(att),((max(att))/100))), las = 1, tck =+0.01,cex.axis=0.7)

##}else{
#Date
#Deixar especificar o eixo date at!
#axis.Date(1, at = seq(as.Date(startdate),as.Date(enddate),"years"),cex.axis=0.7)
#axis.Date(1, at = seq(as.Date(startdate),as.Date(enddate),Freq),labels = FALSE, tcl = -0.2,cex.axis=0.7)
##at=1:nn
##seqq=round(seq(min(att), max(att),((max(att))/100)))
##axis.Date(1, at = axisxdate[seqq],labels = axisxdate[seqq], tcl = -0.4,cex.axis=0.7)

##}
if(plotYt==TRUE){
legend("topright", c("Time Series","Smoothed Mean","95 CI"),
lty=arg[[7]],lwd=arg[[8]],col=arg[[2]],cex=arg[[9]], bty="n")
}else{
legend("topright", c("Smoothed Mean","95 CI"),
lty=arg[[7]][-1],lwd=arg[[8]][-1],col=arg[[2]][-1],cex=arg[[9]], bty="n")
}

} #End Smooth

if(Proc=="Filter"){
#Filtering:
nn=length(Yt)
nsamplex=dim(posts)[1]
if(model=="PEM"){nn=length(Break)-1}
filpar1=matrix(0,nn,nsamplex)
#samples=1
#if(is.null(dim(posts))){samples=500}
for(j in 1:nsamplex){
filparaux=FilteringF(StaPar=posts[j,],formula=formula,data=data,model=model,pz=pz,
a0=a0,b0=b0,distl=distl,splot=FALSE)
filpar1[,j]=((filparaux[2,]/ filparaux[1,]))
}
filpar=apply(filpar1,1,mean)
#filparmedian=apply(filpar1,1,median)]
print(filparaux)
alpha=1-ci
filparp1=apply(filpar1,1,function(x) quantile(x,probs=c(alpha/2),na.rm=TRUE))    #perc alpha/2
filparp2=apply(filpar1,1,function(x) quantile(x,probs=c(1-(alpha/2)),na.rm=TRUE))   #perc 1-alpha/2

## Graph:
ytm=matrix(0,nn,4)
if(model=="PEM"){}else{ytm[,1]=Yt}
#aplicar uma transf.
#sums=fits[[1]]
ytm[,1]=ytm[,1]^(transf)
ytm[,2]=filpar^(transf)
ytm[,3]=filparp1^(transf)
ytm[,4]=filparp2^(transf)
minyt=min(ytm)
maxyt=max(ytm)
if(is.null(axisxdate)){
#Seq. ordem das obs.
at=1:nn
}else{
#Date
#Deixar especificar o eixo date at!
#at = seq(as.Date(startdate),as.Date(enddate),Freq)
at=axisxdate
}
d=at
#if(is.null(startdate)==FALSE & is.null(enddate)==FALSE){
#dat=seq(d[1], d[length(d)], by="month")}
if(is.null(arg[[6]])|is.null(arg[[6]])){
plot(at,ytm[,2],xlab=arg[[3]],ylab=arg[[4]],type=arg[[1]],axes=FALSE,ylim=c(minyt,maxyt),col="white",cex=arg[[9]])
seq1=at
seq2=sort(at,decreasing = TRUE)
xxx=sort(at,decreasing = TRUE)
polygon(c(at, xxx),c((ytm[,4]),rev((ytm[,3]))),ylim=c(minyt,maxyt),col=arg[[2]][3],border=arg[[2]][3])
if(plotYt==TRUE){
par(new=TRUE)
plot(at,ytm[,1],xlab=arg[[3]],ylab=arg[[4]],type=arg[[1]],axes=TRUE,ylim=c(minyt,maxyt),lty=arg[[7]][1],
lwd=arg[[8]][1],col=arg[[2]][1],cex=arg[[9]])
}
par(new=TRUE)
plot(at,ytm[,2],xlab=arg[[3]],ylab=arg[[4]],type=arg[[1]],axes=TRUE,ylim=c(minyt,maxyt),lty=arg[[7]][2],
lwd=arg[[8]][2],col=arg[[2]][2],cex=arg[[9]])
}else{
plot(at,ytm[,2],xlab=arg[[3]],ylab=arg[[4]],type=arg[[1]],axes=FALSE,ylim=arg[[6]],col="white",cex=arg[[9]])
seq1=at
seq2=sort(at,decreasing = TRUE)
xxx=sort(at,decreasing = TRUE)
polygon(c(at, xxx),c((ytm[,4]),rev((ytm[,3]))),ylim=arg[[6]],col=arg[[2]][3],border=arg[[2]][3])
if(plotYt==TRUE){
par(new=TRUE)
plot(at,ytm[,1],xlab=arg[[3]],ylab=arg[[4]],type=arg[[1]],ylim=arg[[6]],axes=TRUE,lty=arg[[7]][1],
lwd=arg[[8]][2],col=arg[[2]][1],cex=arg[[9]])
}
par(new=TRUE)
plot(at,ytm[,2],xlab=arg[[3]],ylab=arg[[4]],type=arg[[1]],ylim=arg[[6]],axes=TRUE,lty=arg[[7]][2],
lwd=arg[[8]][2],col=arg[[2]][2],cex=arg[[9]])

}

if(plotYt==TRUE){
legend("topright", c("Time Series","Filtered Mean Level","95 CI"),
lty=arg[[7]],lwd=arg[[8]],col=arg[[2]],cex=arg[[9]], bty="n")
}else{
legend("topright", c("Filtered Mean","95 CI"),
lty=arg[[7]][-1],lwd=arg[[8]][-1],col=arg[[2]][-1],cex=arg[[9]], bty="n")

}

} #EndFilter




} #End PlotF
##########################################################################################

