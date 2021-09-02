################################################################################
##
## Likelihood Function
##
################################################################################
##
##

#'@noRd
LikeF2 <- function(Yt,Xt,Zt=NULL,Event=NULL,Break=NULL,na.action="na.omit",
                   model="Poisson",StaPar=NULL,a0=0.01,b0=0.01,amp=FALSE){

#StaPar=par
# DataFrame:  
#dataf<-data  
#dataf<-dataf[all.vars(formula)]
#Dataframe data
#if(length(all.vars(formula))> dim(data)[2])stop("Check the formula and data.")
#if(is.data.frame(data)==FALSE)stop("The argument needs to be a data frame.")
#attach(dataf)
oldoptions <-options(warn=-1)
on.exit(options(oldoptions)) 

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
#iik=2
#Event<-Eventdd
#Break<-Breakdd
#Xtdd=NULL
#Ztdd=NULL
#if(is.null(pz)){
#if(dim(dataf)[2]>2){
#nnnd=dim(dataf)[1]
#ppd=dim(dataf)[2]-iik
#Xtdd=matrix(0,nnnd,ppd)
#for(i in 1:ppd){
##Xt[,i]=get(names(dataf)[i+2])
#Xtdd[,i]=dataf[[names(dataf)[i+iik]]] 

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
#Xt[,i]=get(names(dataf)[i+1])
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
#Xt[,i]=get(names(dataf)[i+1])
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
################################################################################

#detach(dataf)
#print(Yt)
#print(Xt)
#print(Zt)

     if (a0 <= 0) stop("Bad input value for a0")
     if (b0 <= 0) stop("Bad input value for b0")
     if (is.null(Yt))stop("Bad input Yt")
     if (is.vector(Yt)==FALSE)stop("Bad input for Yt")
     if (is.vector(Xt))stop("Bad input for Xt. Put as a matrix.")
     #print(StaPar)
      if (is.null(StaPar))stop("Bad input for StaPar")
     if (is.data.frame(StaPar))stop("Bad input for StaPar")
     if (is.vector(StaPar)==FALSE)stop("Bad input for StaPar")
   #  if (model!="Poisson" && model!="Normal"&& model!="Laplace"&&model!="GED"&&
    # model!="Gamma"&& model!="GGamma"&& model!="Weibull")stop("Bad input for model")
     if (sum(length(which(is.na(Yt))))>0)stop("Bad input Yt")
     if(is.null(Xt)==FALSE){if (sum(length(which(is.na(Xt))))>0)stop("Bad input Xt")}
     if(is.null(Xt)==FALSE){if(is.matrix(Xt)==FALSE){Xt=as.matrix(Xt)}}
     if(is.null(Zt)==FALSE){if(is.matrix(Zt)==FALSE){Zt=as.matrix(Xt)}}
     if(StaPar[1]==0)("Bad input for the static parameter w: value outside the parameter space.")
     if (model=="Poisson" || model=="Normal" || model=="Laplace" || model=="GED"||   # Begin TS Models
     model=="Gamma" || model=="GGamma" || model=="Weibull"){

     n<-length(Yt)
     #Likelihood:
     l <- array(0,c(n,1))
   		#	psi   <- exp((par[1]/2)*(lgamma(3/par[1])-lgamma(1/par[1])) )
      if(is.null(Xt)==FALSE){
       if(is.null(Zt)==FALSE){
        dbeta=dim((Xt))[2]    #1
        dteta=dim((Zt))[2]
        dStaPar=length(StaPar)
        Beta=matrix(StaPar[(dStaPar-dbeta-dteta+1):(dStaPar-dteta)],dbeta,1)
        Teta=matrix(StaPar[(dStaPar-dteta+1):(dStaPar)],dteta,1)
        }else{ #2
       # print("CERTOOOOO!")
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
        
        StaPar[1]=exp(-exp(StaPar[1]))
 
#        print(Beta)
#        print(Teta)
      
      #cat("\nSPar=",StaPar)
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
  				    #Poisson
					    jt=t-1
					 #   for(t in 1:n){
				     l[jt] <- lgamma((Yt[jt] + att[jt])) -
			      	lgamma(Yt[jt]+1)+
		 	     	att[jt] * log(btt[jt]) -lgamma(att[jt]) - (Yt[jt] + att[jt])*(log(1 + btt[jt]))
          #  } #end for t

    				  at[t] <- att[t-1]+(Yt[t-1])
					    bt[t] <- btt[t-1]+(1)
					    } #end for t
					  }else{
				       if (min(Yt)<0)stop("Bad input Yt. Negative values.")
				       #dbeta=dim((Xt))[2] 
					  for(t in 2:(n+1)){
		          att[t-1] <- StaPar[1]*at[t-1]
    				  btt[t-1] <- StaPar[1]*bt[t-1]*exp(-(Xt[t-1,1:dbeta]%*%Beta))
			      #Poisson
					    jt=t-1
					 #   for(t in 1:n){
				     l[jt] <- lgamma((Yt[jt] + att[jt])) -
			      	lgamma(Yt[jt]+1)+
		 	     	att[jt] * log(btt[jt]) -lgamma(att[jt]) - (Yt[jt] + att[jt])*(log(1 + btt[jt]))
          #  } #end for t

    				  at[t] <- att[t-1]+(Yt[t-1])
					    bt[t] <-StaPar[1]*bt[t-1]+(1)*exp((Xt[t-1,1:dbeta]%*%Beta))
					   # cat("\nte=",(Xt[t-1,1:dbeta]%*%Beta))
             } #end for t
           #  cat("at=",at)
            # cat("bt=",bt)
             #cat("\nlikef=",sum(l))
            }
           }
 
 	 				   if(model=="Normal"){
 				   if(is.null(Xt)){
  				  for(t in 2:(n+1)){ #begin for t
  				   att[t-1] <- StaPar[1]*at[t-1]
    				 btt[t-1] <- StaPar[1]*bt[t-1]
    				 at[t] <- att[t-1]+(1/2)
   				   if(is.null(Zt)){
   				   bt[t] <- btt[t-1]+(Yt[t-1]^2)/2
   				   # Normal
				     jt=t-1  
   				   # for(t in 1:n){
				     l[t] <- lgamma((0.5 + att[t])) - 0.5*log(2*3.1428) 
             +att[t] * log(btt[t])-lgamma(att[t])- (0.5 + att[t])*(log(0.5*((Yt[t])^2) + btt[t]))
            #} #end for t
 				     }else{
					   if(dteta==0){tt=1}else{tt=1:dteta}
  				   bt[t] <- btt[t-1]+(((Yt[t-1]-(Zt[t-1,tt]%*%Teta))^2)/2)
  				   # Normal
				     jt=t-1  
            dteta=dim((Zt))[2]
            if(dteta==0){tt=1}else{tt=1:dteta}
            #for(t in 1:n){
				     l[jt] <- lgamma((0.5 + att[jt])) - 0.5*log(2*3.1428) +att[jt] * log(btt[jt])
             -lgamma(att[jt])- (0.5 + att[jt])*(log(0.5*((Yt[jt]-(Zt[jt,tt]%*%Teta))^2) + btt[jt]))
            #} #end for t
           }
         }    
        }else{
					   for(t in 2:(n+1)){ #begin for t
  				     att[t-1] <- StaPar[1]*at[t-1]
    				   btt[t-1] <- StaPar[1]*bt[t-1]*exp(-(Xt[t-1,1:dbeta]%*%Beta))
    				   at[t] <- att[t-1]+(1/2)
     				   if(is.null(Zt)){ 
               bt[t] <- StaPar[1]*bt[t-1]+((Yt[t-1]^2)/2)*exp((Xt[t-1,1:dbeta]%*%Beta)) 
            # Normal
	           jt=t-1 
             #for(t in 1:n){
				     l[jt] <- lgamma((0.5 + att[jt])) - 0.5*log(2*3.1428) +att[jt] * log(btt[jt])
             -lgamma(att[jt])- (0.5 + att[jt])*(log(0.5*((Yt[jt])^2) + btt[jt]))
               #} #end for t
    #        cat("\nte=",btt)   
               }else{
   					   if(dteta==0){tt=1}else{tt=1:dteta}
					     bt[t] <- StaPar[1]*bt[t-1]+(((Yt[t-1]-(Zt[t-1,tt]%*%Teta))^2)/2)*exp((Xt[t-1,1:dbeta]%*%Beta))
					     # Normal
				       jt=t-1 
            dteta=dim((Zt))[2]
            if(dteta==0){tt=1}else{tt=1:dteta}
            #for(t in 1:n){
				     l[jt] <- lgamma((0.5 + att[jt])) - 0.5*log(2*3.1428) +att[jt] * log(btt[jt])
             -lgamma(att[jt])- (0.5 + att[jt])*(log(0.5*((Yt[jt]-(Zt[jt,tt]%*%Teta))^2) + btt[jt]))
            #} #end for t
         }  
     } #end for t     
     } 
   }
   	       if(model=="Laplace"){
   	       if(is.null(Xt)){
      	    for(t in 2:(n+1)){   #begin for t
      	     att[t-1] <- StaPar[1]*at[t-1]
    				 btt[t-1] <- StaPar[1]*bt[t-1]
    				 at[t] <- att[t-1]+(1)
    				 if(is.null(Zt)){
					   bt[t] <- btt[t-1]+sqrt(2)*abs(Yt[t-1])
					    # Laplace
				     jt=t-1 
				     l[jt] <- lgamma(att[jt]+1) + log(1/sqrt(2)) + att[jt] * log(btt[jt])
             -lgamma(att[jt])- (1 + att[jt])*(log(sqrt(2)*abs(Yt[jt]) + btt[jt]))
				      }else{ 
					    if(dteta==0){tt=1}else{tt=1:dteta}
              bt[t] <- btt[t-1]+sqrt(2)*abs(Yt[t-1]-(Zt[t-1,tt]%*%Teta))
               # Laplace
				     jt=t-1 
				       dteta=dim((Zt))[2]
              if(dteta==0){tt=1}else{tt=1:dteta}
             l[jt] <- lgamma(att[jt]+1) + log(1/sqrt(2)) + att[jt] * log(btt[jt])-lgamma(att[jt])
              - (1 + att[jt])*(log(sqrt(2)*abs(Yt[jt]-(Zt[jt,tt]%*%Teta)) + btt[jt]))
             }     
					  } #end for t
					  }else{
					   for(t in 2:(n+1)){   #begin for t
      	      att[t-1] <- StaPar[1]*at[t-1]
    				  btt[t-1] <- StaPar[1]*bt[t-1]*exp(-(Xt[t-1,1:dbeta]%*%Beta))
    				  at[t] <- att[t-1]+(1)
    				  if(is.null(Zt)){
					    bt[t] <- StaPar[1]*bt[t-1]+(sqrt(2)*abs(Yt[t-1]))*exp((Xt[t-1,1:dbeta]%*%Beta))
				     # Laplace
				     jt=t-1 
				     l[jt] <- lgamma(att[jt]+1) + log(1/sqrt(2)) + att[jt] * log(btt[jt])
             -lgamma(att[jt])- (1 + att[jt])*(log(sqrt(2)*abs(Yt[jt]) + btt[jt]))
					    }else{ 
 					    if(dteta==0){tt=1}else{tt=1:dteta}
              bt[t] <- StaPar[1]*bt[t-1]+(sqrt(2)*abs(Yt[t-1]-(Zt[t-1,tt]%*%Teta)))*exp((Xt[t-1,1:dbeta]%*%Beta))
           # Laplace
				     jt=t-1 
		         dteta=dim((Zt))[2]
              if(dteta==0){tt=1}else{tt=1:dteta}
             l[jt] <- lgamma(att[jt]+1) + log(1/sqrt(2)) + att[jt] * log(btt[jt])
             -lgamma(att[jt])- (1 + att[jt])*(log(sqrt(2)*abs(Yt[jt]-(Zt[jt,tt]%*%Teta)) + btt[jt]))

             } 
					   } #end for t
					  }
 				   }
           if(model=="GED"){
            StaPar[2]=exp(StaPar[2])+1e-3
           if(is.null(Xt)){
     	#		  at[1]    <- 1/((1-StaPar[1])*StaPar[2])
   		#	    bt[1]    <- StaPar[1]/(StaPar[1]*StaPar[2]+abs(StaPar[1]-1)*(StaPar[2]^2)

			    	for(t in 2:(n+1)){  #begin for t
             att[t-1] <- StaPar[1]*at[t-1]
    				 btt[t-1] <- StaPar[1]*bt[t-1]
	   				 psi   <- ((gamma(3/StaPar[2]))/gamma(1/StaPar[2]))^(StaPar[2]/2)
    				 at[t] <- att[t-1]+(1/StaPar[2])
    				 if(is.null(Zt)){
					   bt[t] <- btt[t-1]+((abs(Yt[t-1]))^StaPar[2])*psi
					    # GED
				     jt=t-1 
				     # for(t in 1:n){
				     l[jt] <- lgamma((1/StaPar[2] + att[jt])) +
			      	log(StaPar[2]/2) + ((1/2)*lgamma((3/StaPar[2]))-(3/2)*lgamma((1/StaPar[2])) +
		 	     	 att[jt] * log(btt[jt]) -lgamma(att[jt])) - (1/StaPar[2] + att[jt]) * (log(((abs(Yt[jt]))^StaPar[2])*psi + btt[jt]))
  #          }
					   }else{  
             bt[t] <- btt[t-1]+((abs(Yt[t-1]-(0)))^StaPar[2])*psi   
              # GED
				     jt=t-1 
               #dteta=dim((Zt))[2]
            if(dteta==0){tt=1}else{tt=1:dteta}
        #      for(t in 1:n){
             l[jt] <- lgamma((1/StaPar[2] + att[jt])) +
			      	log(StaPar[2]/2) + ((1/2)*lgamma((3/StaPar[2]))-(3/2)*lgamma((1/StaPar[2])) +
		 	     	 att[jt] * log(btt[jt]) -lgamma(att[jt])) - (1/StaPar[2] + att[jt]) * (log(((abs(Yt[jt]-(Zt[jt,tt]%*%Teta)))^StaPar[2])*psi + btt[jt]))
		 	     	#  }  
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
					    # GED
				     jt=t-1 
				     # for(t in 1:n){
				     l[jt] <- lgamma((1/StaPar[2] + att[jt])) +
			      	log(StaPar[2]/2) + ((1/2)*lgamma((3/StaPar[2]))-(3/2)*lgamma((1/StaPar[2])) +
		 	     	 att[jt] * log(btt[jt]) -lgamma(att[jt])) - (1/StaPar[2] + att[jt]) * (log(((abs(Yt[jt]))^StaPar[2])*psi + btt[jt]))
  #          }
					   }else{
					   if(dteta==0){tt=1}else{tt=1:dteta}
              bt[t] <- StaPar[1]*bt[t-1]+(((abs(Yt[t-1]-(Zt[t-1,tt]%*%Teta)))^StaPar[2])*psi)*exp((Xt[t-1,1:dbeta]%*%Beta))
               # GED
				     jt=t-1 
				       #dteta=dim((Zt))[2]
            if(dteta==0){tt=1}else{tt=1:dteta}
        #      for(t in 1:n){
             l[jt] <- lgamma((1/StaPar[2] + att[jt])) +
			      	log(StaPar[2]/2) + ((1/2)*lgamma((3/StaPar[2]))-(3/2)*lgamma((1/StaPar[2])) +
		 	     	 att[jt] * log(btt[jt]) -lgamma(att[jt])) - (1/StaPar[2] + att[jt]) * (log(((abs(Yt[jt]-(Zt[jt,tt]%*%Teta)))^StaPar[2])*psi + btt[jt]))
		 	     	#  }
            }    
					  }  #end for t
           }
          }

 				   if(model=="Gamma"){
 				     if (min(Yt)<0)stop("Bad input Yt. Negative values.")
  		        StaPar[2]=exp(StaPar[2])
 				   if(is.null(Xt)){
  				  for(t in 2:(n+1)){   #begin for t
  				   att[t-1] <- StaPar[1]*at[t-1]
    				 btt[t-1] <- StaPar[1]*bt[t-1]
 					   # Gamma
				    # jt=t-1
    #         for(t in 1:n){
				     #l[jt] <- lgamma(att[jt]+StaPar[2])+(StaPar[2]-1)*log(Yt[jt])+att[jt] * log(btt[jt]) 
             #-lgamma(StaPar[2]) -lgamma(att[jt]) - (StaPar[2] + att[jt])*(log(Yt[jt] + btt[jt]))
    #        } #end for t
    				 at[t] <- att[t-1]+(StaPar[2])
					   bt[t] <- btt[t-1]+(Yt[t-1])
					  }   #end for t
					  }else{
				      if (min(Yt)<0)stop("Bad input Yt. Negative values.")
  				  for(t in 2:(n+1)){   #begin for t
  				   att[t-1] <- StaPar[1]*at[t-1]
    				 btt[t-1] <- StaPar[1]*bt[t-1]*exp(-(Xt[t-1,1:dbeta]%*%Beta))
            # Gamma
				   #  jt=t-1
    #         for(t in 1:n){
				    # l[jt] <- lgamma(att[jt]+StaPar[2])+(StaPar[2]-1)*log(Yt[jt])+att[jt] * log(btt[jt]) 
             #-lgamma(StaPar[2]) -lgamma(att[jt]) - (StaPar[2] + att[jt])*(log(Yt[jt] + btt[jt]))
    #        } #end for t
    				 at[t] <- att[t-1]+(StaPar[2])
					   bt[t] <- StaPar[1]*bt[t-1]+(Yt[t-1])*exp((Xt[t-1,1:dbeta]%*%Beta))
					  }   #end for t

					  }
 				   }
			    if(model=="GGamma"){
				    if (min(Yt)<0)stop("Bad input Yt. Negative values.")
		         StaPar[2]=exp(StaPar[2])
   		       StaPar[3]=exp(StaPar[3])
			    if(is.null(Xt)){
 				    for(t in 2:(n+1)){  #begin for t
 				     att[t-1] <- StaPar[1]*at[t-1]
    				 btt[t-1] <- StaPar[1]*bt[t-1]
    				 # GGamma
				#     jt=t-1
				#     l[jt] <- lgamma((StaPar[2] + att[jt])) -lgamma(att[jt])+att[jt]*log(btt[jt]) + log(StaPar[3]) + (StaPar[3]*StaPar[2]-1)*log(Yt[jt]) - lgamma(StaPar[2])+ (-att[jt]-StaPar[2])*log((Yt[jt]^StaPar[3])+btt[jt])
    				 at[t] <- att[t-1]+(StaPar[2])
					   bt[t] <- btt[t-1]+(Yt[t-1]^StaPar[3])
					  }
					  }else{
				      if (min(Yt)<0)stop("Bad input Yt. Negative values.")
 				    for(t in 2:(n+1)){  #begin for t
 				     att[t-1] <- StaPar[1]*at[t-1]
    				 btt[t-1] <- StaPar[1]*bt[t-1]*exp(-(Xt[t-1,1:dbeta]%*%Beta))
    				 # GGamma
				#     jt=t-1
				 #    l[jt] <- lgamma((StaPar[2] + att[jt])) -lgamma(att[jt])+att[jt]*log(btt[jt]) + log(StaPar[3]) + (StaPar[3]*StaPar[2]-1)*log(Yt[jt]) - lgamma(StaPar[2])+ (-att[jt]-StaPar[2])*log((Yt[jt]^StaPar[3])+btt[jt])				     
    				 at[t] <- att[t-1]+(StaPar[2])
					   bt[t] <- StaPar[1]*bt[t-1]+(Yt[t-1]^StaPar[3])**exp((Xt[t-1,1:dbeta]%*%Beta))
					  }

					  }
 				   }
 				   if(model=="Weibull"){
 				     if (min(Yt)<0)stop("Bad input Yt. Negative values.")
		        StaPar[2]=exp(StaPar[2])
 				   if(is.null(Xt)){
  				  for(t in 2:(n+1)){  #begin for t
  				   att[t-1] <- StaPar[1]*at[t-1]
    				 btt[t-1] <- StaPar[1]*bt[t-1]
    				 # Weibull
				     jt=t-1
				    l[jt] <- lgamma((1 + att[jt])) + log(StaPar[2]) + (StaPar[2]-1)*log(Yt[jt]) - lgamma(att[jt])+ att[jt]*log(btt[jt]) + (-1 - att[jt])*log(Yt[jt]^StaPar[2] + btt[jt])
    				 at[t] <- att[t-1]+(1)
					   bt[t] <- btt[t-1]+(Yt[t-1]^StaPar[2])
					  }  #end for t
					  }else{
					    if (min(Yt)<0)stop("Bad input Yt. Negative values.")
  				  for(t in 2:(n+1)){  #begin for t
  				   att[t-1] <- StaPar[1]*at[t-1]
    				 btt[t-1] <- StaPar[1]*bt[t-1]*exp(-(Xt[t-1,1:dbeta]%*%Beta))
    				 # Weibull
				     jt=t-1
				     l[jt] <- lgamma((1 + att[jt])) + log(StaPar[2]) + (StaPar[2]-1)*log(Yt[jt]) - lgamma(att[jt])+ att[jt]*log(btt[jt]) + (-1 - att[jt])*log(Yt[jt]^StaPar[2] + btt[jt])
    				 at[t] <- att[t-1]+(1)
					   bt[t] <- StaPar[1]*bt[t-1]+(Yt[t-1]^StaPar[2])*exp(Xt[t-1,1:dbeta]%*%Beta)
					  }  #end for t

					  }
 				   }
return(-sum(l))
}#End TS Models

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
      #print(StaPar)
     if(StaPar[1]==0)("Bad input for the static parameter w: value outside the parameter space.")
  #   if(StaPar[1]==0){StaPar[1]=1e-10}

   			n<-length(Yt)
   		#	psi   <- exp((par[1]/2)*(lgamma(3/par[1])-lgamma(1/par[1])) )
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
      
      StaPar[1]=exp(-exp(StaPar[1]))
      
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
    #Likelihood:
        l <- array(0,c(n,1))
	      if(model1=="Gamma"){
	         StaPar[2]=exp(StaPar[2])
		      if (min(Yt)<0)stop("Bad input Yt. Negative values.")
		      
 				   if(is.null(Xt)){
  				  for(t in 2:(n+1)){   #begin for t
  				   att[t-1] <- StaPar[1]*at[t-1]
    				 btt[t-1] <- StaPar[1]*bt[t-1]
				   # Gamma
				     jt=t-1 
	         #Gamma
           #for(t in 1:n){
				     l[jt] <- lgamma(att[jt]+StaPar[2])+(StaPar[2]-1)*log(Yt[jt])+att[jt]*log(btt[jt])-lgamma(StaPar[2]) -lgamma(att[jt])-(StaPar[2] + att[jt])*(log(Yt[jt] + btt[jt]))
           # } #end for t
    				 at[t] <- att[t-1]+(StaPar[2])
					   bt[t] <- btt[t-1]+(Yt[t-1])
					  }   #end for t
					  }else{
					 # print("Ok!")
					  if (min(Yt)<0)stop("Bad input Yt. Negative values.")
  				  for(t in 2:(n+1)){   #begin for t
  				   att[t-1] <- StaPar[1]*at[t-1]
    				 btt[t-1] <- StaPar[1]*bt[t-1] #*exp(-(Xt[t-1,1:dbeta]%*%Beta))
         # Gamma
				     jt=t-1            
         #    for(t in 1:n){
         #  print("Ok!")
          # cat("\nte=",l)
          # cat("\nte=",btt)
				     l[jt] <- lgamma(att[jt]+StaPar[2])+(StaPar[2]-1)*log(Yt[jt])+(-StaPar[2]*Xt[jt,1:dbeta]%*%Beta)+att[jt] * log(btt[jt])-lgamma(StaPar[2]) -lgamma(att[jt]) - (StaPar[2] + att[jt])*(log(Yt[jt]*exp(-Xt[jt,1:dbeta]%*%Beta) + btt[jt]))
          #  } #end for t

    				 at[t] <- att[t-1]+(StaPar[2])
					   bt[t] <- StaPar[1]*bt[t-1]+(Yt[t-1])*exp((-Xt[t-1,1:dbeta]%*%Beta))
             btmu[t] <-StaPar[1]*bt[t-1]+(Yt[t-1])*exp(-(Xt[t-1,1:dbeta]%*%Beta))
    #        cat("\nte=",btt)
					  }   #end for t
					  }   #end  if
 				   }
			    
 				   if(model1=="Weibull"){
		         StaPar[2]=exp(StaPar[2])
  		      #Likelihood:
          #  l <- array(0,c(n,1))
 				   if (min(Yt)<0)stop("Bad input Yt. Negative values.")
 				   if(is.null(Xt)){
  				  for(t in 2:(n+1)){  #begin for t
  				   att[t-1] <- StaPar[1]*at[t-1]
    				 btt[t-1] <- StaPar[1]*bt[t-1]
    				 at[t] <- att[t-1]+(1)
					   bt[t] <- btt[t-1]+(Yt[t-1]^StaPar[2])
					    # Weibull
				     jt=t-1 
          #  for(t in 1:n){
				     l[jt] <- lgamma((1 + att[jt])) + log(StaPar[2]) + (StaPar[2]-1)*log(Yt[jt])-lgamma(att[jt])+ att[jt]*log(btt[jt]) + (-1 - att[jt])*log(Yt[jt]^StaPar[2] + btt[jt])               
         #    } #end for t
    #        cat("\nte=",btt)
					  }  #end for t
					  }else{
					  if (min(Yt)<0)stop("Bad input Yt. Negative values.")
					  #print("Ok!")
  				  for(t in 2:(n+1)){  #begin for t
  				   att[t-1] <- StaPar[1]*at[t-1]
    				 btt[t-1] <- StaPar[1]*bt[t-1] #*exp(-(Xt[t-1,1:dbeta]%*%Beta)*StaPar[2])
    				 at[t] <- att[t-1]+(1)
					   bt[t] <- StaPar[1]*bt[t-1]+(Yt[t-1]^StaPar[2])*exp(-StaPar[2]*Xt[t-1,1:dbeta]%*%Beta)
             btmu[t] <- StaPar[1]*bt[t-1]+(Yt[t-1]^StaPar[2])*exp(-StaPar[2]*Xt[t-1,1:dbeta]%*%Beta) 
             #*exp(-(Xt[t-1,1:dbeta]%*%Beta))
              # Weibull
				     jt=t-1 
            # for(t in 1:n){
				     l[jt] <- lgamma(1 + att[jt])+log(StaPar[2])+(StaPar[2]-1)*log(Yt[jt])+(-StaPar[2]*Xt[jt,1:dbeta]%*%Beta)-lgamma(att[jt])+att[jt]*log(btt[jt])+(-1 - att[jt])*log(((Yt[jt]^StaPar[2])*exp(-StaPar[2]*Xt[jt,1:dbeta]%*%Beta)) + btt[jt])
           #  } #end for t
    #        cat("\nte=",btt)
					  }  #end for t
					  }
					  #print(att)
					  #print(btt)
 				   } # end if
 				           
 				   
  return(-sum(l))
    
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
     if(StaPar[1]==0)("Bad input for the static parameter w: value outside the parameter space.")
     if (is.null(Event))stop("Bad input Event")
     if (is.null(Break))stop("Bad input Break")
  

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
       StaPar[1]=exp(-exp(StaPar[1]))
       
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
		         #Likelihood:
              l <- array(0,c(n,1))  
 				     if(is.null(Xt)==TRUE){
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
             # PEM
				     jt=t-1 
				     l[jt] <- lgamma(att[jt]+numF[jt])+att[jt] * log(btt[jt])-lgamma(att[jt]) - (numF[jt] + att[jt])*(log(TT[jt] + btt[jt]))
    				 }else{
    				   att[t-1] <- StaPar[1]*at[t-1]
    				   btt[t-1] <- StaPar[1]*bt[t-1]
    				   # PEM
				       jt=t-1
     	         l[jt] <- lgamma(att[jt]+numF[jt])+att[jt] * log(btt[jt])-lgamma(att[jt])  - (numF[jt] + att[jt])*(log(TT[jt] + btt[jt])) 
    				 }
    				 at[t] <- att[t-1]+(numF[t-1])
					   bt[t] <- btt[t-1]+(TT[t-1])
					   btmu[t] <- btt[t-1]+(TT[t-1])
					   # PEM
				     jt=t-1 
#   	         l[jt] <- lgamma(att[jt]+numF[jt])+att[jt] * log(btt[jt])-lgamma(att[jt]) 
 #            - (numF[jt] + att[jt])*(log(TT[jt] + btt[jt]))

					  }   #end for t
					  }else{
					   if (min(Yt)<0)stop("Bad input Yt. Negative values.")
					   #Break=GridP(Yt, Event, nT = NULL)
 				     numF=NumFail(StaPar,Yt,Event,Break,Xt)
 				     TT=TTime(StaPar,Yt,Event,Break,Xt)
 				     XtC=ProdXtChi(StaPar,Yt,Break,Event,Xt)
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
    				 # PH
				     jt=t-1
             l[jt] <- XtC[jt] + lgamma(att[jt] + numF[jt]) + att[jt] * log(btt[jt]) 
             -lgamma(att[jt]) - (numF[jt] + att[jt])*(log(TT[jt] + btt[jt])) 
    				 }else{
    				   att[t-1] <- StaPar[1]*at[t-1]
    				   btt[t-1] <- StaPar[1]*bt[t-1]
 				     # PH
				     jt=t-1
             l[jt] <- XtC[jt] + lgamma(att[jt] + numF[jt]) + att[jt] * log(btt[jt]) 
             -lgamma(att[jt]) - (numF[jt] + att[jt])*(log(TT[jt] + btt[jt])) 
    				 }
    				 at[t] <- att[t-1]+(numF[t-1])
					   bt[t] <- btt[t-1]+(TT[t-1])
					   btmu[t] <- btt[t-1]+(TT[t-1]) 
				     # PH
#				     jt=t-1
 #            l[jt] <- XtC[jt] + lgamma(att[jt] + numF[jt]) + att[jt] * log(btt[jt]) 
  #           -lgamma(att[jt]) - (numF[jt] + att[jt])*(log(TT[jt] + btt[jt])) 
					  }
					  } 
                 #Likelihood:
      #  l <- array(0,c(n,1))
  
 #    if(is.null(Xt)==TRUE){
  #          #PEM
   #         for(t in 1:n){
		#		     l[t] <- lgamma(att[t]+numF[t])+att[t] * log(btt[t])-lgamma(att[t]) - (numF[t] + att[t])*(log(TT[t] + btt[t]))
     #       } #end for t
            
            
          
      #   }else{
       #   #PH
        #  XtC=ProdXtChi(StaPar,Yt,Break,Event,Xt)
         #          for(t in 1:n){
				  #   l[t] <- XtC[t] + lgamma(att[t] + numF[t]) + att[t] * log(btt[t]) -lgamma(att[t]) - (numF[t] + att[t])*(log(TT[t] + btt[t]))
				   
           # }
        # }
             
  return(-sum(l)) 
  }#End PEM/PH Model

}
##########################################################



