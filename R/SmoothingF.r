################################################################################
##
## Smoothing Function: Conditional and Marginal
##
################################################################################

##
##
#'@export
SmoothingF <- function(formula, data,na.action="na.omit",pz=NULL,nBreaks=NULL,
                      model="Poisson",StaPar=NULL,Type="Cond",a0=0.01,b0=0.01,
                        amp=FALSE,samples=1,ci=0.95,splot=FALSE){

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
  Event=NULL
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
 # cat("Yt=",Yt)
#  print(Xt)
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
#Xtdd=matrix(0,nnnd,ppd)
#for(i in 1:ppd){
##Xt[,i]=get(names(dataf)[i+2])
#Xtdd[,i]=dataf[[names(dataf)[i+2]]]  
#}
#Ztdd=matrix(0,nnnd,pz)
#for(j in 1:pz){
##Zt[,j]=get(names(dataf)[j+ppd+2])
#Ztdd[,j]=dataf[[names(dataf)[j+ppd+2]]]
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


if(Type!="Marg" && Type!="Cond")stop("Bad input for Type argument.")
if(is.null(Xt)==FALSE){if(is.matrix(Xt)==FALSE){Xt=as.matrix(Xt)}}
if(is.null(Zt)==FALSE){if(is.matrix(Zt)==FALSE){Zt=as.matrix(Xt)}}

if(Type=="Marg"){ 
if(is.matrix(StaPar)==FALSE)stop("Warning: StaPar must be a matrix, a sample of static parameters!")
################################################################################
nn=length(Yt)
if(model=="PEM"){nn=length(Break)-1}
nsamplex=dim(StaPar)[1]
p=dim(StaPar)[2]
#if(nn==1){nn=length(t(Yt))}   
#Smoothing:
set.seed(1000)
MeanSmoothaux=matrix(0,nn,nsamplex)
SEQ  <- seq(1,nsamplex)
pb   <- txtProgressBar(1, nsamplex, style=3)
TIME <- Sys.time()
message("Running...")
for(j in 1:nsamplex){
MeanSmoothaux[1:nn,j]=SmoothingF(StaPar=StaPar[j,],formula=formula,data=data,pz=pz,nBreaks=nBreaks,model=model,Type="Cond"
,a0=a0,b0=b0,amp=amp,samples=1,ci=ci,splot=FALSE)
Sys.sleep(0.02)
setTxtProgressBar(pb, j)
}
message("Done!")
#MeanSmooth=(MeanSmoothaux)
Meanbayes=apply(MeanSmoothaux,1,mean)
Medianbayes=apply(MeanSmoothaux,1,median)
q1=(1-(ci))/2
q2=ci+(1-(ci))/2
Q1bayes=apply(MeanSmoothaux,1,quantile,probs=q1)
Q2bayes=apply(MeanSmoothaux,1,quantile,probs=q2)  
summarylambda=matrix(0,nn,4)
summarylambda[,1]=Meanbayes
summarylambda[,2]=Medianbayes
summarylambda[,3]=Q1bayes
summarylambda[,4]=Q2bayes
colnames(summarylambda)=c("Mean","Median","Perc1","Perc2")

 if(splot==TRUE){ #Begin Plot
######################################################################
####################################################################
##
## GRAFICO SOMBREADO
##
####################################################################
# dev.new()
n1=length(Medianbayes)
ytm=matrix(0,n1,3)
ytm[,1]=Medianbayes
ytm[,2]=Q1bayes
ytm[,3]=Q2bayes
minyt=min(ytm)-0.1*min(ytm)
maxyt=max(ytm)+0.1*max(ytm)
#at = seq(as.Date("1999/12/02"),as.Date("2000/12/21"),"days") 
##d=as.Date(dat,format="%Y-%m-%d")
#d=at
#dat=seq(d[1], d[length(d)], by="month")
at=1:n1
seq1=seq(1,n1)
seq2=seq(n1,1)
#seq3=array(0,c(n1))
plot.ts(ytm[,1], ylab=expression(paste(hat(mu)[t])),
xlab="t",ylim=c(minyt,maxyt))
title("Estimation of States")
polygon(c(seq1, seq2),c(ytm[,3],rev(ytm[,2])),col="light grey",border="light grey")
lines(ytm[,1])
#lines(log(ytmm[2:(n1),2]-0.00001),lty = c("dashed"),lwd=c(2))
#lines(log(sbe2[2:(n1)]-0.00001),lty = c("dotted"),lwd=c(2))
legend("topright", c("Smoothed Median",ci*100,"% CI"),lty=c(1,1,1),lwd=c(1,1,1),
col=c("black","light grey","white"), cex=0.68, bty="n", pt.cex = 1)          
######################################################################
}#End Splot = TRUE
#cat("\n*****Summaries of the Marginal Distribution of the States*****\n")
#print(summarylambda)
return(list(summarylambda,MeanSmoothaux))

################################################################################   
###############################################################################   
}else{
      if(is.matrix(StaPar))stop("Warning: StaPar must be a vector, a point of static parameters!")
################################################################################   
     if (a0 <= 0) stop("Bad input value for a0")
     if (b0 <= 0) stop("Bad input value for b0")
     if (is.null(Yt))stop("Bad input Yt")
     if (is.vector(Yt)==FALSE)stop("Bad input for Yt")
     if (is.vector(Xt))stop("Bad input for Xt. Put as a matrix.")
     if (is.null(StaPar))stop("Bad input for StaPar")
     if (is.data.frame(StaPar))stop("Bad input for StaPar")
     if (is.vector(StaPar)==FALSE)stop("Bad input for StaPar")
    # if (model!="Poisson" && model!="Normal"&& model!="Laplace"&&model!="GED"&&
     #model!="Gamma"&& model!="GGamma"&& model!="Weibull")stop("Bad input for model")
     if (sum(length(which(is.na(Yt))))>0)stop("Bad input Yt")
     if(is.null(Xt)==FALSE){if (sum(length(which(is.na(Xt))))>0)stop("Bad input Xt")}
     if (ci<=0 | ci>=1)stop("Bad input for ci")
     if (is.integer(samples))stop("Bad input for samples")
     if (ceiling(samples)!=samples)stop("Bad input for samples")
     
     if (model=="Poisson" || model=="Normal" || model=="Laplace" || model=="GED"||   # Begin TS Models
     model=="Gamma" || model=="GGamma" || model=="Weibull"){

     n<-length(Yt)
    # if(is.null(Xt)==FALSE){
    #   if(is.null(dim(Xt))){
     #   dbeta=dim(t(Xt))[1]
     #   dStaPar=length(StaPar)
     #   Beta=matrix(StaPar[(dStaPar-dbeta+1):(dStaPar)],dbeta,1)
      #  }else{
     #   dbeta=dim(Xt)[2]
      #  dStaPar=length(StaPar)
      #  Beta=matrix(StaPar[(dStaPar-dbeta+1):(dStaPar)],dbeta,1)
      #  }
      #}
      if(is.null(Xt)==FALSE){
       if(is.null(Zt)==FALSE){
        dbeta=dim((Xt))[2]    #1
        dteta=dim((Zt))[2]
        dStaPar=length(StaPar)
        Beta=matrix(StaPar[(dStaPar-dbeta-dteta+1):(dStaPar-dteta)],dbeta,1)
        Teta=matrix(StaPar[(dStaPar-dteta+1):(dStaPar)],dteta,1)
        }else{ #2
        dbeta=dim((Xt))[2]    
        dteta=0
        dStaPar=length(StaPar)
        Beta=matrix(StaPar[(dStaPar-dbeta+1):(dStaPar)],dbeta,1)
        Teta=0    

        }
       }
       if(is.null(Xt)==TRUE){ 
          if(is.null(Zt)==FALSE){ #3
           dbeta=0    
           dteta=dim((Zt))[2]
           dStaPar=length(StaPar)
           Teta=matrix(StaPar[(dStaPar-dteta+1):(dStaPar)],dteta,1)
          }else{ Beta=Teta=0 }     #4
        }

      
      if(StaPar[1]==1){StaPar[1]=StaPar[1]-0.001}
      if(StaPar[1]==0){StaPar[1]=StaPar[1]+0.001}

     # cat("SPar=",StaPar)
     # print(Beta)

        mab   <- matrix(0,2,n+1)
				att   <- array(0,c((n),1))
				btt   <- array(0,c((n),1))
				at    <- array(0,c((n+1),1))
				bt    <- array(0,c((n+1),1))

				#Pred:
				at[1] <- a0
				bt[1] <- b0
    #for(t in 2:(n+1)){

          if(model=="Poisson"){
   				   if (min(Yt)<0)stop("Bad input Yt. Negative values.")
   				 # for(t in 2:(n+1)){
             if(is.null(Xt)){
             for(t in 2:(n+1)){  #begin for t
              att[t-1] <- StaPar[1]*at[t-1]
    				  btt[t-1] <- StaPar[1]*bt[t-1]
    				  at[t] <- att[t-1]+(Yt[t-1])
					    bt[t] <- btt[t-1]+(1)
					    } #end for t
					  }else{
				      if (min(Yt)<0)stop("Bad input Yt. Negative values.")
					  for(t in 2:(n+1)){
		          att[t-1] <- StaPar[1]*at[t-1]
    				  btt[t-1] <- StaPar[1]*bt[t-1]*exp(-(Xt[t-1,1:dbeta]%*%Beta))
    				  at[t] <- att[t-1]+(Yt[t-1])
					    bt[t] <-StaPar[1]*bt[t-1]+(1)*exp((Xt[t-1,1:dbeta]%*%Beta))
					   # cat("\nte=",(Xt[t-1,1:dbeta]%*%Beta))
             } #end for t
            }
           }
           
 				 #  if(model=="Normal"){
 				 #  if(is.null(Xt)){
  				#  for(t in 2:(n+1)){ #begin for t
  				 #  att[t-1] <- StaPar[1]*at[t-1]
    			#	 btt[t-1] <- StaPar[1]*bt[t-1]
    			#	 at[t] <- att[t-1]+(1/2)
					 #  bt[t] <- btt[t-1]+(Yt[t-1]^2)/2
					 # }
					 # }else{
					  # for(t in 2:(n+1)){ #begin for t
  				  #   att[t-1] <- StaPar[1]*at[t-1]
    				 #  btt[t-1] <- StaPar[1]*bt[t-1]*exp(-(Xt[t-1,1:dbeta]%*%Beta))
    				 #  at[t] <- att[t-1]+(1/2)
					    # bt[t] <- StaPar[1]*bt[t-1]+((Yt[t-1]^2)/2)*exp((Xt[t-1,1:dbeta]%*%Beta))
					#  }
 				  # } #end for t
 				  # }
 				  
	      if(model=="Normal"){
 				   if(is.null(Xt)){
  				  for(t in 2:(n+1)){ #begin for t
  				   att[t-1] <- StaPar[1]*at[t-1]
    				 btt[t-1] <- StaPar[1]*bt[t-1]
    				 at[t] <- att[t-1]+(1/2)
   				   if(is.null(Zt)){
   				   bt[t] <- btt[t-1]+(Yt[t-1]^2)/2
					   }else{
					   bt[t] <- btt[t-1]+(((Yt[t-1]-(Zt[t-1,1:dteta]%*%Teta))^2)/2)
					        }     
					   }     
					  }else{
					   for(t in 2:(n+1)){ #begin for t
  				     att[t-1] <- StaPar[1]*at[t-1]
    				   btt[t-1] <- StaPar[1]*bt[t-1]*exp(-(Xt[t-1,1:dbeta]%*%Beta))
    				   at[t] <- att[t-1]+(1/2)
    				   
    				   if(is.null(Zt)){ 
               bt[t] <- StaPar[1]*bt[t-1]+((Yt[t-1]^2)/2)*exp((Xt[t-1,1:dbeta]%*%Beta)) 
  				    # btmu[t] <-(StaPar[1]*bt[t-1]+((Yt[t-1]^2)/2)*exp((Xt[t-1,1:dbeta]%*%Beta)))*exp(-(Xt[t-1,1:dbeta]%*%Beta))
              }else{
					     bt[t] <- StaPar[1]*bt[t-1]+(((Yt[t-1]-(Zt[t-1,1:dteta]%*%Teta))^2)/2)*exp((Xt[t-1,1:dbeta]%*%Beta))
 				       #btmu[t] <-(StaPar[1]*bt[t-1]+(((Yt[t-1]-(Zt[t-1,1:dteta]%*%Teta))^2)/2)*exp((Xt[t-1,1:dbeta]%*%Beta)))*exp(-(Xt[t-1,1:dbeta]%*%Beta))

                    }
              } #end for t     
					  } 
					  }

 				   
   	     #  if(model=="Laplace"){
   	      # if(is.null(Xt)){
      	 #   for(t in 2:(n+1)){   #begin for t
      	  #   att[t-1] <- StaPar[1]*at[t-1]
    			#	 btt[t-1] <- StaPar[1]*bt[t-1]
    			#	 at[t] <- att[t-1]+(1)
					#   bt[t] <- btt[t-1]+sqrt(2)*abs(Yt[t-1])
					#  } #end for t
					#  }else{
					 #  for(t in 2:(n+1)){   #begin for t
      	   #   att[t-1] <- StaPar[1]*at[t-1]
    				#  btt[t-1] <- StaPar[1]*bt[t-1]*exp(-(Xt[t-1,1:dbeta]%*%Beta))
    				#  at[t] <- att[t-1]+(1)
					  #  bt[t] <- StaPar[1]*bt[t-1]+(sqrt(2)*abs(Yt[t-1]))*exp((Xt[t-1,1:dbeta]%*%Beta))
					  # } #end for t
					  #}
 				   #}
 				   
       if(model=="Laplace"){
   	       if(is.null(Xt)){
      	    for(t in 2:(n+1)){   #begin for t
      	     att[t-1] <- StaPar[1]*at[t-1]
    				 btt[t-1] <- StaPar[1]*bt[t-1]
    				 at[t] <- att[t-1]+(1)
    				 if(is.null(Zt)){
					   bt[t] <- btt[t-1]+sqrt(2)*abs(Yt[t-1])
				      }else{bt[t] <- btt[t-1]+sqrt(2)*abs(Yt[t-1]-(Zt[t-1,1:dteta]%*%Teta))
                   }
					  } #end for t
					  }else{
					   for(t in 2:(n+1)){   #begin for t
      	      att[t-1] <- StaPar[1]*at[t-1]
    				  btt[t-1] <- StaPar[1]*bt[t-1]*exp(-(Xt[t-1,1:dbeta]%*%Beta))
    				  at[t] <- att[t-1]+(1)
    				  if(is.null(Zt)){
					    bt[t] <- StaPar[1]*bt[t-1]+(sqrt(2)*abs(Yt[t-1]))*exp((Xt[t-1,1:dbeta]%*%Beta))
					   # btmu[t] <-(StaPar[1]*bt[t-1]+(sqrt(2)*abs(Yt[t-1]))*exp((Xt[t-1,1:dbeta]%*%Beta)))*exp(-(Xt[t-1,1:dbeta]%*%Beta))
					    }else{bt[t] <- StaPar[1]*bt[t-1]+(sqrt(2)*abs(Yt[t-1]-(Zt[t-1,1:dteta]%*%Teta)))*exp((Xt[t-1,1:dbeta]%*%Beta))
  				    #      btmu[t] <-(StaPar[1]*bt[t-1]+(sqrt(2)*abs(Yt[t-1]-(Zt[t-1,1:dteta]%*%Teta)))*exp((Xt[t-1,1:dbeta]%*%Beta)))*exp(-(Xt[t-1,1:dbeta]%*%Beta))

                   }
					   } #end for t
					  }
 				   }
 				      
 				   
        #   if(model=="GED"){
         #  if(is.null(Xt)){
     		#	  at[1]    <- 1/((1-StaPar[1])*StaPar[2])
   			 #   bt[1]    <- StaPar[1]/(StaPar[1]*StaPar[2]+abs(StaPar[1]-1)*(StaPar[2]^2))
			   # 	for(t in 2:(n+1)){  #begin for t
         #    att[t-1] <- StaPar[1]*at[t-1]
    			#	 btt[t-1] <- StaPar[1]*bt[t-1]
	   		#		 psi   <- ((gamma(3/StaPar[2]))/gamma(1/StaPar[2]))^(StaPar[2]/2)
    		#		 at[t] <- att[t-1]+(1/StaPar[2])
				#	   bt[t] <- btt[t-1]+((abs(Yt[t-1]))^StaPar[2])*psi
				#	  }  #end for t
				#	  }else{
     		#	  at[1]    <- 1/((1-StaPar[1])*StaPar[2])
   			 #   bt[1]    <- StaPar[1]/(StaPar[1]*StaPar[2]+abs(StaPar[1]-1)*(StaPar[2]^2))
			   # 	for(t in 2:(n+1)){  #begin for t
          #   att[t-1] <- StaPar[1]*at[t-1]
    			#	 btt[t-1] <- StaPar[1]*bt[t-1]*exp(-(Xt[t-1,1:dbeta]%*%Beta))
	   			#	 psi   <- ((gamma(3/StaPar[2]))/gamma(1/StaPar[2]))^(StaPar[2]/2)
    			#	 at[t] <- att[t-1]+(1/StaPar[2])
					 #  bt[t] <- StaPar[1]*bt[t-1]+(((abs(Yt[t-1]))^StaPar[2])*psi)*exp((Xt[t-1,1:dbeta]%*%Beta))
					#  }  #end for t
          # }
 				  # }
 				  
 				 if(model=="GED"){
           if(is.null(Xt)){
     			  at[1]    <- 1/((1-StaPar[1])*StaPar[2])
   			    bt[1]    <- StaPar[1]/(StaPar[1]*StaPar[2]+abs(StaPar[1]-1)*(StaPar[2]^2))
			    	for(t in 2:(n+1)){  #begin for t
             att[t-1] <- StaPar[1]*at[t-1]
    				 btt[t-1] <- StaPar[1]*bt[t-1]
	   				 psi   <- ((gamma(3/StaPar[2]))/gamma(1/StaPar[2]))^(StaPar[2]/2)
    				 at[t] <- att[t-1]+(1/StaPar[2])
    				 if(is.null(Zt)){
					   bt[t] <- btt[t-1]+((abs(Yt[t-1]))^StaPar[2])*psi
					   }else{  bt[t] <- btt[t-1]+((abs(Yt[t-1]-(Zt[t-1,1:dteta]%*%Teta)))^StaPar[2])*psi     
                  }
					  }  #end for t
					  }else{
     			  at[1]    <- 1/((1-StaPar[1])*StaPar[2])
   			    bt[1]    <- StaPar[1]/(StaPar[1]*StaPar[2]+abs(StaPar[1]-1)*(StaPar[2]^2))
			    	for(t in 2:(n+1)){  #begin for t
             att[t-1] <- StaPar[1]*at[t-1]
    				 btt[t-1] <- StaPar[1]*bt[t-1]*exp(-(Xt[t-1,1:dbeta]%*%Beta))
	   				 psi   <- ((gamma(3/StaPar[2]))/gamma(1/StaPar[2]))^(StaPar[2]/2)
    				 at[t] <- att[t-1]+(1/StaPar[2])
    				 if(is.null(Zt)){
					   bt[t] <- StaPar[1]*bt[t-1]+(((abs(Yt[t-1]))^StaPar[2])*psi)*exp((Xt[t-1,1:dbeta]%*%Beta))
		         #btmu[t] <-(StaPar[1]*bt[t-1]+(((abs(Yt[t-1]))^StaPar[2])*psi)*exp((Xt[t-1,1:dbeta]%*%Beta)))*exp(-(Xt[t-1,1:dbeta]%*%Beta))
					   }else{
              bt[t] <- StaPar[1]*bt[t-1]+(((abs(Yt[t-1]-(Zt[t-1,1:dteta]%*%Teta)))^StaPar[2])*psi)*exp((Xt[t-1,1:dbeta]%*%Beta))
   		       #  btmu[t] <-(StaPar[1]*bt[t-1]+(((abs(Yt[t-1]-(Zt[t-1,1:dteta]%*%Teta)))^StaPar[2])*psi)*exp((Xt[t-1,1:dbeta]%*%Beta)))*exp(-(Xt[t-1,1:dbeta]%*%Beta))
                  }
					  }  #end for t
           }
          }
 				    
 				   if(model=="Gamma"){
 				      if (min(Yt)<0)stop("Bad input Yt. Negative values.")
 				   if(is.null(Xt)){
  				  for(t in 2:(n+1)){   #begin for t
  				   att[t-1] <- StaPar[1]*at[t-1]
    				 btt[t-1] <- StaPar[1]*bt[t-1]
    				 at[t] <- att[t-1]+(StaPar[2])
					   bt[t] <- btt[t-1]+(Yt[t-1])
					  }   #end for t
					  }else{
				      if (min(Yt)<0)stop("Bad input Yt. Negative values.")
  				  for(t in 2:(n+1)){   #begin for t
  				   att[t-1] <- StaPar[1]*at[t-1]
    				 btt[t-1] <- StaPar[1]*bt[t-1]*exp(-(Xt[t-1,1:dbeta]%*%Beta))
    				 at[t] <- att[t-1]+(StaPar[2])
					   bt[t] <- StaPar[1]*bt[t-1]+(Yt[t-1])*exp((Xt[t-1,1:dbeta]%*%Beta))
					  }   #end for t

					  }
 				   }
			    if(model=="GGamma"){
			      if (min(Yt)<0)stop("Bad input Yt. Negative values.")
			    if(is.null(Xt)){
 				    for(t in 2:(n+1)){  #begin for t
 				     att[t-1] <- StaPar[1]*at[t-1]
    				 btt[t-1] <- StaPar[1]*bt[t-1]
    				 at[t] <- att[t-1]+(StaPar[2])
					   bt[t] <- btt[t-1]+(Yt[t-1]^StaPar[3])
					  }
					  }else{
				      if (min(Yt)<0)stop("Bad input Yt. Negative values.")
 				    for(t in 2:(n+1)){  #begin for t
 				     att[t-1] <- StaPar[1]*at[t-1]
    				 btt[t-1] <- StaPar[1]*bt[t-1]*exp(-(Xt[t-1,1:dbeta]%*%Beta))
    				 at[t] <- att[t-1]+(StaPar[2])
					   bt[t] <- StaPar[1]*bt[t-1]+(Yt[t-1]^StaPar[3])**exp((Xt[t-1,1:dbeta]%*%Beta))
					  }

					  }
 				   }
 				   if(model=="Weibull"){
 				     if (min(Yt)<0)stop("Bad input Yt. Negative values.")
 				   if(is.null(Xt)){
  				  for(t in 2:(n+1)){  #begin for t
  				   att[t-1] <- StaPar[1]*at[t-1]
    				 btt[t-1] <- StaPar[1]*bt[t-1]
    				 at[t] <- att[t-1]+(1)
					   bt[t] <- btt[t-1]+(Yt[t-1]^StaPar[2])
					  }  #end for t
					  }else{
				      if (min(Yt)<0)stop("Bad input Yt. Negative values.")
  				  for(t in 2:(n+1)){  #begin for t
  				   att[t-1] <- StaPar[1]*at[t-1]
    				 btt[t-1] <- StaPar[1]*bt[t-1]*exp(-(Xt[t-1,1:dbeta]%*%Beta))
    				 at[t] <- att[t-1]+(1)
					   bt[t] <- StaPar[1]*bt[t-1]+(Yt[t-1]^StaPar[2])*exp(Xt[t-1,1:dbeta]%*%Beta)
					  }  #end for t

					  }
 				   }

#### Smoothing Step:
 #        output <-matrix(0,n,4)
         mu <- array(0,c(n,1))
         mdata <-matrix(0,n,samples)

         if(is.null(Xt)){
          for(ii in 1:samples){
            lambda <- array(0,c(n,1))
            mu <- array(0,c(n,1))
            lambda[n] <- rgamma(1,att[n],btt[n])
            mu[n] <- lambda[n]
   				 for(i in n:2) {
   				   lambda[i-1] <- rgamma(1,(1-StaPar[1])*at[i-1],bt[i-1])+StaPar[1]*lambda[i]
   				   mu[i-1] <- lambda[i-1]
           }
           mdata[,ii]=lambda
         }

         }else{
         for(ii in 1:samples){
         lambda <- array(0,c(n,1))
         mu <- array(0,c(n,1))
         lambda[n] <- rgamma(1,att[n],btt[n])
         mu[n] <- lambda[n]*exp(Xt[n,1:dbeta]%*%Beta)
   				 for(i in n:2) {
   				   lambda[i-1] <- rgamma(1,(1-StaPar[1])*at[i-1],bt[i-1])+StaPar[1]*lambda[i]
   				   mu[i-1] <- lambda[i-1]*exp(Xt[i-1,1:dbeta]%*%Beta)
           }
         mdata[,ii]=mu
         }
        }

         if(samples==1){
         return(mdata)
         }else{
         Mean=apply(mdata,1,mean) #mean
         Median=apply(mdata,1,median) #median
         alpha=1-ci
         Perc1=apply(mdata,1,function(x) quantile(x,probs=c(alpha/2)))    #perc alpha/2
         Perc2=apply(mdata,1,function(x) quantile(x,probs=c(1-(alpha/2))))   #perc 1-alpha/2
         if(splot==TRUE){ #Begin Plot
          ###################################################################### 
           #n1=length(Median)
           #ytm=matrix(0,n1,3)
           #ytm[,1]=Median
           #ytm[,2]=Perc1
           #ytm[,3]=Perc2
           #minyt=min(ytm)
           #maxyt=max(ytm)
           #at = seq(as.Date("1999/12/02"),as.Date("2000/12/21"),"days") 
           ##d=as.Date(dat,format="%Y-%m-%d")
           #d=at
           #dat=seq(d[1], d[length(d)], by="month")
          # at=1:n1
           # dev.new()
           #plot(at,ytm[,1],xlab="t",ylab=expression(paste(hat(lambda)[t])),type='l',
           #ylim=c(minyt,maxyt),lty=c(1),lwd=c(2),col=c("black"),main="Estimates of States")
           #par(new=TRUE)
           #plot(at,ytm[,2],xlab="t",ylab=expression(paste(hat(lambda)[t])),type='l',
           #ylim=c(minyt,maxyt),lty=c(2),lwd=c(2),col=c("blue"))
           #par(new=TRUE)
           #plot(at,ytm[,3],xlab="t",ylab=expression(paste(hat(lambda)[t])),type='l',
           #ylim=c(minyt,maxyt),lty=c(2),lwd=c(2),col=c("blue"))
           ##axis.Date(1,at=dat,labels=dat, las = 2) 
           ##axis(2, at =seq(min(Yt), max(Yt), 1), las = 1, tck = +0.01,cex.axis=0.7)
           ##mtext(side = 2, "Yt", line = 3.0)  #Y-axis label
           #legend("topright", c("Smoothed Median","Smoothed Lower CI",
           #"Smoothed Upper CI"),lty=c(1,2,2),lwd=c(2,2,2),
           # col=c("black","blue","blue"), cex=0.68, bty="n", pt.cex = 1)
            
          ####################################################################
          ##
          ## GRAFICO SOMBREADO
          ##
          ####################################################################
        #  dev.new()
           n1=length(Median)
           ytm=matrix(0,n1,3)
           ytm[,1]=Median
           ytm[,2]=Perc1
           ytm[,3]=Perc2
           minyt=min(ytm)-0.1*min(ytm)
           maxyt=max(ytm)+0.1*max(ytm)
           #at = seq(as.Date("1999/12/02"),as.Date("2000/12/21"),"days") 
           ##d=as.Date(dat,format="%Y-%m-%d")
           #d=at
           #dat=seq(d[1], d[length(d)], by="month")
           at=1:n1
            seq1=seq(1,n1)
            seq2=seq(n1,1)
            #seq3=array(0,c(n1))
            plot.ts(ytm[,1], ylab=expression(paste(hat(mu)[t])),
            xlab="t",ylim=c(minyt,maxyt))
            title("Estimation of States")
            polygon(c(seq1, seq2),c(ytm[,3],rev(ytm[,2])),col="light grey",border="light grey")
            lines(ytm[,1])
            #lines(log(ytmm[2:(n1),2]-0.00001),lty = c("dashed"),lwd=c(2))
            #lines(log(sbe2[2:(n1)]-0.00001),lty = c("dotted"),lwd=c(2))
           legend("topright", c("Smoothed Median",ci*100,"% CI"),lty=c(1,1,1),lwd=c(1,1,1),
           col=c("black","light grey","white"), cex=0.68, bty="n", pt.cex = 1)            
         } #End Plot
         return(data.frame(Mean,Median,Perc1,Perc2))
     }#End Else
      #   return(data.frame(Mean,Median,Perc1,Perc2))        
   }#End Time Series Models
   
   if(model=="SRGamma" || model=="SRWeibull"){                # Begin SR
     if(model=="SRGamma"){model1="Gamma"}
     if(model=="SRWeibull"){model1="Weibull"}
     if (a0 <= 0) stop("Bad input value for a0")
     if (b0 <= 0) stop("Bad input value for b0")
     if (is.null(Yt))stop("Bad input Yt")
     if (is.vector(Yt)==FALSE)stop("Bad input for Yt")
     if (is.vector(Xt))stop("Bad input for Xt. Put as a matrix.")
     if (is.null(StaPar))stop("Bad input for StaPar")
     if (is.data.frame(StaPar))stop("Bad input for StaPar")
     if (is.vector(StaPar)==FALSE)stop("Bad input for StaPar")
     if (model1!="Gamma"&&model1!="Weibull")stop("Bad input for model")
     if (sum(length(which(is.na(Yt))))>0)stop("Bad input Yt")
     if(is.null(Xt)==FALSE){if (sum(length(which(is.na(Xt))))>0)stop("Bad input Xt")}
     if (ci<=0 | ci>=1)stop("Bad input for ci")
     if (is.integer(samples))stop("Bad input for samples")
     if (ceiling(samples)!=samples)stop("Bad input for samples")


     n<-length(Yt)
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
      if(StaPar[1]==1){StaPar[1]=StaPar[1]-0.001}
      if(StaPar[1]==0){StaPar[1]=StaPar[1]+0.001}

     # cat("SPar=",StaPar)
     # print(Beta)

        mab   <- matrix(0,2,n+1)
				att   <- array(0,c((n),1))
				btt   <- array(0,c((n),1))
				at    <- array(0,c((n+1),1))
				bt    <- array(0,c((n+1),1))
				btmu    <- array(0,c((n+1),1))

				#Pred:
				at[1] <- a0
				bt[1] <- b0

		   if(model1=="Gamma"){
 				     if (min(Yt)<0)stop("Bad input Yt. Negative values.")
 				   if(is.null(Xt)){
  				  for(t in 2:(n+1)){   #begin for t
  				   att[t-1] <- StaPar[1]*at[t-1]
    				 btt[t-1] <- StaPar[1]*bt[t-1]
    				 at[t] <- att[t-1]+(StaPar[2])
					   bt[t] <- btt[t-1]+(Yt[t-1])
					  }   #end for t
					  }else{
					  if (min(Yt)<0)stop("Bad input Yt. Negative values.")
  				  for(t in 2:(n+1)){   #begin for t
  				   att[t-1] <- StaPar[1]*at[t-1]
    				 btt[t-1] <- StaPar[1]*bt[t-1] #*exp(-(Xt[t-1,1:dbeta]%*%Beta))
    				 at[t] <- att[t-1]+(StaPar[2])
					   bt[t] <- StaPar[1]*bt[t-1]+(Yt[t-1])*exp((-Xt[t-1,1:dbeta]%*%Beta))
             btmu[t] <-StaPar[1]*bt[t-1]+(Yt[t-1])*exp(-(Xt[t-1,1:dbeta]%*%Beta))

					  }   #end for t

					  }
 				   }
			    
 				   if(model1=="Weibull"){
 				   if (min(Yt)<0)stop("Bad input Yt. Negative values.")
 				   if(is.null(Xt)){
  				  for(t in 2:(n+1)){  #begin for t
  				   att[t-1] <- StaPar[1]*at[t-1]
    				 btt[t-1] <- StaPar[1]*bt[t-1]
    				 at[t] <- att[t-1]+(1)
					   bt[t] <- btt[t-1]+(Yt[t-1]^StaPar[2])
					  }  #end for t
					  }else{
					  if (min(Yt)<0)stop("Bad input Yt. Negative values.")
  				  for(t in 2:(n+1)){  #begin for t
  				   att[t-1] <- StaPar[1]*at[t-1]
    				 btt[t-1] <- StaPar[1]*bt[t-1] #*exp(-(Xt[t-1,1:dbeta]%*%Beta)*StaPar[2])
    				 at[t] <- att[t-1]+(1)
					   bt[t] <- StaPar[1]*bt[t-1]+(Yt[t-1]^StaPar[2])*exp(-Xt[t-1,1:dbeta]%*%Beta*StaPar[2])
             btmu[t] <-StaPar[1]*bt[t-1]+(Yt[t-1]^StaPar[2])*exp(-Xt[t-1,1:dbeta]%*%Beta*StaPar[2]) #*exp(-(Xt[t-1,1:dbeta]%*%Beta))

					  }  #end for t

					  }
 				   } # end for
#### Smoothing Step:
 #        output <-matrix(0,n,4)
         mu <- array(0,c(n,1))
         mdata <-matrix(0,n,samples)

   if(model1=="Weibull"){ 
         if(is.null(Xt)){
          for(ii in 1:samples){
            lambda <- array(0,c(n,1))
            mu <- array(0,c(n,1))
            lambda[n] <- rgamma(1,att[n],btt[n])
            #mu[n] <- lambda[n]
            mu[n]=((1/(lambda[n]))^(1/StaPar[2]))*exp(lgamma(1+(1/StaPar[2]))) # mean
            mu[n]=((1/lambda[n])^(1/StaPar[2]))*(log(2)^(1/StaPar[2])) #median

   				 for(i in n:2) {
   				   lambda[i-1] <- rgamma(1,(1-StaPar[1])*at[i-1],bt[i-1])+StaPar[1]*lambda[i]
   				   #mu[i-1] <- lambda[i-1]
             mu[i-1]=((1/(lambda[i-1]))^(1/StaPar[2]))*exp(lgamma(1+(1/StaPar[2]))) # mean
             mu[i-1]=((1/lambda[i-1])^(1/StaPar[2]))*(log(2)^(1/StaPar[2]))  #median

           }
           mdata[,ii]=mu
         }

         }else{
         for(ii in 1:samples){
         lambda <- array(0,c(n,1))
         mu <- array(0,c(n,1))
         lambda[n] <- rgamma(1,att[n],btt[n])
        # mu[n] <- lambda[n]*exp(Xt[n,1:dbeta]%*%Beta)
         mu[n]=((1/(lambda[n]*exp(-StaPar[2]*Xt[n,1:dbeta]%*%Beta)))^(1/StaPar[2]))*exp(lgamma(1+(1/StaPar[2]))) # mean
         mu[n]=((1/(lambda[n]*exp(-StaPar[2]*Xt[n,1:dbeta]%*%Beta)))^(1/StaPar[2]))*(log(2)^(1/StaPar[2]))  #median

   				 for(i in n:2) {
   				   lambda[i-1] <- rgamma(1,(1-StaPar[1])*at[i-1],bt[i-1])+StaPar[1]*lambda[i]
   				  # mu[i-1] <- lambda[i-1]*exp(Xt[i-1,1:dbeta]%*%Beta)
             mu[i-1]=((1/(lambda[i-1]*exp(-StaPar[2]*Xt[i-1,1:dbeta]%*%Beta)))^(1/StaPar[2]))*exp(lgamma(1+(1/StaPar[2]))) # mean
             mu[i-1]=((1/(lambda[i-1]*exp(-StaPar[2]*Xt[i-1,1:dbeta]%*%Beta)))^(1/StaPar[2]))*(log(2)^(1/StaPar[2]))   #median

           }
         mdata[,ii]=mu
         }
        }
      } #end model
      
         if(model1=="Gamma"){ 
         if(is.null(Xt)){
          for(ii in 1:samples){
            lambda <- array(0,c(n,1))
            mu <- array(0,c(n,1))
            lambda[n] <- rgamma(1,att[n],btt[n])
            #mu[n] <- lambda[n]
            mu[n]=((1/lambda[n])*(StaPar[2]))   #mean
            mu[n]=((1/lambda[n])*(StaPar[2]))*(2/3)  #median

   				 for(i in n:2) {
   				   lambda[i-1] <- rgamma(1,(1-StaPar[1])*at[i-1],bt[i-1])+StaPar[1]*lambda[i]
   				   #mu[i-1] <- lambda[i-1]
             mu[i-1]=((1/lambda[i-1])*(StaPar[2])) #mean
             mu[i-1]=((1/lambda[i-1])*(StaPar[2]))*(2/3) #median
           }
           mdata[,ii]=mu
         }

         }else{
         for(ii in 1:samples){
         lambda <- array(0,c(n,1))
         mu <- array(0,c(n,1))
         lambda[n] <- rgamma(1,att[n],btt[n])
        # mu[n] <- lambda[n]*exp(Xt[n,1:dbeta]%*%Beta)
           # mu[n]=((1/(lambda[n]*exp(-Xt[n,1:dbeta]%*%Beta)))*(StaPar[2]))     #mean
            mu[n]=((1/(lambda[n]*exp(-Xt[n,1:dbeta]%*%Beta)))*(StaPar[2]))*(2/3)     #median

   				 for(i in n:2) {
   				   lambda[i-1] <- rgamma(1,(1-StaPar[1])*at[i-1],bt[i-1])+StaPar[1]*lambda[i]
            # mu[i-1]=((1/(lambda[i-1]*exp(-Xt[i-1,1:dbeta]%*%Beta)))*(StaPar[2]))  #mean
             mu[i-1]=((1/(lambda[i-1]*exp(-Xt[i-1,1:dbeta]%*%Beta)))*(StaPar[2]))*(2/3)  #median
 
           }
         mdata[,ii]=mu
         }
        }
      } #end model

         if(samples==1){
         return(mdata)
         }else{
         Mean=apply(mdata,1,mean) #mean
         Median=apply(mdata,1,median) #median
         alpha=1-ci
         Perc1=apply(mdata,1,function(x) quantile(x,probs=c(alpha/2)))    #perc alpha/2
         Perc2=apply(mdata,1,function(x) quantile(x,probs=c(1-(alpha/2))))   #perc 1-alpha/2
  #       return(data.frame(Mean,Median,Perc1,Perc2))
         if(splot==TRUE){ #Begin Plot
          ###################################################################### 
          # n1=length(Median)
          # ytm=matrix(0,n1,3)
          # ytm[,1]=Median
          # ytm[,2]=Perc1
          # ytm[,3]=Perc2
          # minyt=min(ytm)
          # maxyt=max(ytm)
          # #at = seq(as.Date("1999/12/02"),as.Date("2000/12/21"),"days") 
          # ##d=as.Date(dat,format="%Y-%m-%d")
           ##d=at
           ##dat=seq(d[1], d[length(d)], by="month")
           #at=1:n1
           # dev.new()
           #plot(at,ytm[,1],xlab="t",ylab=expression(paste(hat(lambda)[t])),type='l',
           #ylim=c(minyt,maxyt),lty=c(1),lwd=c(2),col=c("black"),main="Estimates of States")
           #par(new=TRUE)
           #plot(at,ytm[,2],xlab="t",ylab=expression(paste(hat(lambda)[t])),type='l',
           #ylim=c(minyt,maxyt),lty=c(2),lwd=c(2),col=c("blue"))
           #par(new=TRUE)
           #plot(at,ytm[,3],xlab="t",ylab=expression(paste(hat(lambda)[t])),type='l',
           #ylim=c(minyt,maxyt),lty=c(2),lwd=c(2),col=c("blue"))
           ##axis.Date(1,at=dat,labels=dat, las = 2) 
           ##axis(2, at =seq(min(Yt), max(Yt), 1), las = 1, tck = +0.01,cex.axis=0.7)
           ##mtext(side = 2, "Yt", line = 3.0)  #Y-axis label
           #legend("topright", c("Smoothed Median","Smoothed Lower CI",
           #"Smoothed Upper CI"),lty=c(1,2,2),lwd=c(2,2,2),
           # col=c("black","blue","blue"), cex=0.68, bty="n", pt.cex = 1)
            
          ####################################################################
          ##
          ## GRAFICO SOMBREADO
          ##
          ####################################################################
          # dev.new()
           n1=length(Median)
           ytm=matrix(0,n1,3)
           ytm[,1]=Median
           ytm[,2]=Perc1
           ytm[,3]=Perc2
           minyt=min(ytm)-0.1*min(ytm)
           maxyt=max(ytm)+0.1*max(ytm)
           #at = seq(as.Date("1999/12/02"),as.Date("2000/12/21"),"days") 
           ##d=as.Date(dat,format="%Y-%m-%d")
           #d=at
           #dat=seq(d[1], d[length(d)], by="month")
           at=1:n1
            seq1=seq(1,n1)
            seq2=seq(n1,1)
            #seq3=array(0,c(n1))
            plot.ts(ytm[,1], ylab=expression(paste(hat(mu)[t])),
            xlab="t",ylim=c(minyt,maxyt))
            title("Estimation of States")
            polygon(c(seq1, seq2),c(ytm[,3],rev(ytm[,2])),col="light grey",border="light grey")
            lines(ytm[,1])
            #lines(log(ytmm[2:(n1),2]-0.00001),lty = c("dashed"),lwd=c(2))
            #lines(log(sbe2[2:(n1)]-0.00001),lty = c("dotted"),lwd=c(2))
           legend("topright", c("Smoothed Median",ci*100,"% CI"),lty=c(1,1,1),lwd=c(1,1,1),
           col=c("black","light grey","white"), cex=0.68, bty="n", pt.cex = 1)            
            
         } #End Plot
         return(data.frame(Mean,Median,Perc1,Perc2))
     }#End Else
   } # End SR Models


   if(model=="PEM"){                                   #Begin PEM/PH Model
        if (a0 <= 0) stop("Bad input value for a0")
     if (b0 <= 0) stop("Bad input value for b0")
     if (is.null(Yt))stop("Bad input Yt")
     if (is.vector(Yt)==FALSE)stop("Bad input for Yt")
     if (is.vector(Xt))stop("Bad input for Xt. Put as a matrix.")
     if (is.null(StaPar))stop("Bad input for StaPar")
     if (is.data.frame(StaPar))stop("Bad input for StaPar")
     if (is.vector(StaPar)==FALSE)stop("Bad input for StaPar")
     if (model!="PEM")stop("Bad input for model")
     if (sum(length(which(is.na(Yt))))>0)stop("Bad input Yt")
     if(is.null(Xt)==FALSE){if (sum(length(which(is.na(Xt))))>0)stop("Bad input Xt")}
     if (ci<=0 | ci>=1)stop("Bad input for ci")
     if (is.integer(samples))stop("Bad input for samples")
     if (ceiling(samples)!=samples)stop("Bad input for samples")
     if (is.null(Event))stop("Bad input Event")
     if (is.null(Break))stop("Bad input Break")
     if (samples<=0)stop("Bad input samples")

     n<-length(Break)-1
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
      if(StaPar[1]==1){StaPar[1]=StaPar[1]-0.000000001}
      if(StaPar[1]==0){StaPar[1]=StaPar[1]+0.000000001}

        mab   <- matrix(0,2,n+1)
				att   <- array(0,c((n),1))
				btt   <- array(0,c((n),1))
				at    <- array(0,c((n+1),1))
				bt    <- array(0,c((n+1),1))
				btmu  <- array(0,c((n+1),1))

				#Pred:
				at[1] <- a0
				bt[1] <- b0

 				     if (min(Yt)<0)stop("Bad input Yt. Negative values.")
 				     if(is.null(Xt)){
 				     numF=NumFail(StaPar,Yt,Event,Break,Xt=NULL)
 				     TT=TTime(StaPar,Yt,Event,Break,Xt=NULL)
  				   for(t in 2:(n+1)){   #begin for t
  				   if(amp==TRUE){
	           d=diff(Break)
 				     tdif<- diff(unique(Yt[Event == 1]))
 				     lamp=tdif[length(tdif)]
 				     d[length(d)]=lamp
             m=mean(d)+1
             z=d/(m)
    				 att[t-1] <- (StaPar[1]^z[t-1])*at[t-1]
    				 btt[t-1] <- (StaPar[1]^z[t-1])*bt[t-1]
    				 }else{
    				   att[t-1] <- StaPar[1]*at[t-1]
    				   btt[t-1] <- StaPar[1]*bt[t-1]
    				 }    				 
             at[t] <- att[t-1]+(numF[t-1])
					   bt[t] <- btt[t-1]+(TT[t-1])
					   btmu[t] <- btt[t-1]+(TT[t-1])
					  }   #end for t
					  }else{
					   if (min(Yt)<0)stop("Bad input Yt. Negative values.")
 				     numF=NumFail(StaPar,Yt,Event,Break,Xt)
 				     TT=TTime(StaPar,Yt,Event,Break,Xt)
  				  for(t in 2:(n+1)){   #begin for t
  				   if(amp==TRUE){
	           d=diff(Break)
 				     tdif<- diff(unique(Yt[Event == 1]))
 				     lamp=tdif[length(tdif)]
 				     d[length(d)]=lamp
             m=mean(d)+1
             z=d/(m)
    				 att[t-1] <- (StaPar[1]^z[t-1])*at[t-1]
    				 btt[t-1] <- (StaPar[1]^z[t-1])*bt[t-1]
    				 }else{
    				   att[t-1] <- StaPar[1]*at[t-1]
    				   btt[t-1] <- StaPar[1]*bt[t-1]
    				 }    				 
             at[t] <- att[t-1]+(numF[t-1])
					   bt[t] <- btt[t-1]+(TT[t-1])
					   btmu[t] <- btt[t-1]+(TT[t-1]) 
					  }
					  }
					  
  #### Smoothing Step:
         mu <- array(0,c(n,1))
         mdata <-matrix(0,n,samples)
          for(ii in 1:samples){
            lambda <- array(0,c(n,1))
            mu <- array(0,c(n,1))
            lambda[n] <- rgamma(1,att[n],btt[n])
            #lambda[n] <- att[n]/btt[n]
            mu[n]=lambda[n] # mean

   				 for(i in n:2) {
   				   lambda[i-1] <- rgamma(1,(1-StaPar[1])*at[i-1],bt[i-1])+StaPar[1]*lambda[i]
             mu[i-1]=lambda[i-1]  # mean
           }
           mdata[,ii]=mu
         }
         if(samples==1){
         return(mdata)
         }else{
         Mean=apply(mdata,1,mean) #mean
         Median=apply(mdata,1,median) #median
         alpha=1-ci
         Perc1=apply(mdata,1,function(x) quantile(x,probs=c(alpha/2)))    #perc alpha/2
         Perc2=apply(mdata,1,function(x) quantile(x,probs=c(1-(alpha/2))))   #perc 1-alpha/2
         if(splot==TRUE){ #Begin Plot
          ###################################################################### 
      #     n1=length(Median)
       #    ytm=matrix(0,n1,3)
        #   ytm[,1]=Median
        #   ytm[,2]=Perc1
         #  ytm[,3]=Perc2
          # minyt=min(ytm)
          # maxyt=max(ytm)
          # #at = seq(as.Date("1999/12/02"),as.Date("2000/12/21"),"days") 
          # ##d=as.Date(dat,format="%Y-%m-%d")
          # #d=at
          # #dat=seq(d[1], d[length(d)], by="month")
           #at=1:n1
           # dev.new()
           #plot(at,ytm[,1],xlab="t",ylab=expression(paste(hat(lambda)[t])),type='s',
           #ylim=c(minyt,maxyt),lty=c(1),lwd=c(2),col=c("black"),main="Estimates of States")
           #par(new=TRUE)
           #plot(at,ytm[,2],xlab="t",ylab=expression(paste(hat(lambda)[t])),type='s',
           #ylim=c(minyt,maxyt),lty=c(2),lwd=c(2),col=c("blue"))
           #par(new=TRUE)
           #plot(at,ytm[,3],xlab="t",ylab=expression(paste(hat(mu)[t])),type='s',
           #ylim=c(minyt,maxyt),lty=c(2),lwd=c(2),col=c("blue"))
           ##axis.Date(1,at=dat,labels=dat, las = 2) 
           ##axis(2, at =seq(min(Yt), max(Yt), 1), las = 1, tck = +0.01,cex.axis=0.7)
           ##mtext(side = 2, "Yt", line = 3.0)  #Y-axis label
           #legend("topright", c("Smoothed Median","Smoothed Lower CI",
           #"Smoothed Upper CI"),lty=c(1,2,2),lwd=c(2,2,2),
           # col=c("black","blue","blue"), cex=0.68, bty="n", pt.cex = 1)
            
          ####################################################################
          ##
          ## GRAFICO SOMBREADO
          ##
          ####################################################################
         #  dev.new()
           n1=length(Median)
           ytm=matrix(0,n1,3)
           ytm[,1]=Median
           ytm[,2]=Perc1
           ytm[,3]=Perc2
           minyt=min(ytm)
           maxyt=max(ytm)
           #at = seq(as.Date("1999/12/02"),as.Date("2000/12/21"),"days") 
           ##d=as.Date(dat,format="%Y-%m-%d")
           #d=at
           #dat=seq(d[1], d[length(d)], by="month")
           at=1:n1
            seq1=seq(1,n1)
            seq2=seq(n1,1)
            #seq3=array(0,c(n1))
            plot.ts(ytm[,1], ylab=expression(paste(hat(mu)[t])),
            xlab="t",ylim=c(minyt,maxyt))
            title("Estimation of States")
            polygon(c(seq1, seq2),c(ytm[,3],rev(ytm[,2])),col="light grey",border="light grey")
            lines(ytm[,1])
            #lines(log(ytmm[2:(n1),2]-0.00001),lty = c("dashed"),lwd=c(2))
            #lines(log(sbe2[2:(n1)]-0.00001),lty = c("dotted"),lwd=c(2))
           legend("topright", c("Smoothed Median",ci*100,"% CI"),lty=c(1,1,1),lwd=c(1,1,1),
           col=c("black","light grey","white"), cex=0.68, bty="n", pt.cex = 1)            

         } #End Plot
         return(data.frame(Mean,Median,Perc1,Perc2))
     }#End Else

   }#End PEM/PH Model
################################################################################   
}#End IfElse Cond.
 }
##########################################################

