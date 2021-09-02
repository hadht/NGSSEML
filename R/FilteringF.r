################################################################################
##
## Filtering Function
##
################################################################################
##
##
#'@export
FilteringF<- function(formula, data,na.action="na.omit",pz=NULL,
nBreaks=NULL,model="Poisson",StaPar=NULL,a0=0.01,b0=0.01,amp=FALSE,
distl="PRED",splot=FALSE){
#print(data)
#Alterar o nome para FilteringF

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
   # Break=GridP(Y, Event, nT = nBreaks)
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
#  cat("Yt=",Yt)
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
#Dataframe data
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
#Ytdd=dataf[[names(dataf)[1]]]

#Xtdd=NULL
#Ztdd=NULL
#if(is.null(pz)){
#if(dim(dataf)[2]>1){
#nnnd=dim(dataf)[1]
#ppd=dim(dataf)[2]-1
#Xtdd=matrix(0,nnnd,ppd)
#for(i in 1:ppd){
##Xt[,i]=get(names(dataf)[i+1])
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
  

     if (a0 <= 0) stop("Bad input value for a0")
     if (b0 <= 0) stop("Bad input value for b0")
     if (is.null(Yt))stop("Bad input Yt")
     if (is.vector(Yt)==FALSE)stop("Bad input for Yt")
     if (distl!="PRED" && distl!="FILTER")stop("Bad input for distl")
     if (is.vector(Xt))stop("Bad input for Xt. Put as a matrix.")
     if (is.null(StaPar))stop("Bad input for StaPar")
     if (is.data.frame(StaPar))stop("Bad input for StaPar")
     if (is.vector(StaPar)==FALSE)stop("Bad input for StaPar")
     if(is.null(Xt)==FALSE){if(is.matrix(Xt)==FALSE){Xt=as.matrix(Xt)}}
     if(is.null(Zt)==FALSE){if(is.matrix(Zt)==FALSE){Zt=as.matrix(Xt)}}

     if (model=="Poisson" || model=="Normal" || model=="Laplace" || model=="GED"||   # Begin TS Models
     model=="Gamma" || model=="GGamma" || model=="Weibull"){
     if (is.null(Event)==FALSE)stop("Bad input Event for this model")
     if (is.null(Break)==FALSE)stop("Bad input Break for this model")
     if (amp==TRUE)stop("Bad input for amp for this model")
     if (model=="Poisson" || model=="Gamma" || model=="GGamma" || model=="Weibull"){
     if(is.null(Zt)==FALSE)stop("Bad input for Zt for this model")}

     
     #if (model!="Poisson" && model!="Normal"&& model!="Laplace"&&model!="GED"&&
    # model!="Gamma"&& model!="GGamma"&& model!="Weibull")stop("Bad input for model")
     if (sum(length(which(is.na(Yt))))>0)stop("Bad input Yt")
     if(is.null(Xt)==FALSE){if (sum(length(which(is.na(Xt))))>0)stop("Bad input Xt")}  
     if(StaPar[1]==0)stop("Bad input for the static parameter w: value outside the parameter space.")
     if(StaPar[1]==0){StaPar[1]=StaPar[1]+0.001}


   			n<-length(Yt)
   		#	psi   <- exp((par[1]/2)*(lgamma(3/par[1])-lgamma(1/par[1])) )
 #    if(is.null(Xt)==FALSE){
  #     if(is.null(dim(Xt))){
   #     dbeta=dim(t(Xt))[1]
    #    dStaPar=length(StaPar)
     #   Beta=matrix(StaPar[(dStaPar-dbeta+1):(dStaPar)],dbeta,1)
      #  }else{
       # dbeta=dim(Xt)[2]
       # dStaPar=length(StaPar)
       # Beta=matrix(StaPar[(dStaPar-dbeta+1):(dStaPar)],dbeta,1)
       # }
      #}
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
					  for(t in 2:(n+1)){
					    if (min(Yt)<0)stop("Bad input Yt. Negative values.")
		          att[t-1] <- StaPar[1]*at[t-1]
    				  btt[t-1] <- StaPar[1]*bt[t-1]*exp(-(Xt[t-1,1:dbeta]%*%Beta))
    				  at[t] <- att[t-1]+(Yt[t-1])
					    bt[t] <-StaPar[1]*bt[t-1]+(1)*exp((Xt[t-1,1:dbeta]%*%Beta))
					    btmu[t] <-(StaPar[1]*bt[t-1]+(1)*exp((Xt[t-1,1:dbeta]%*%Beta)))*exp(-(Xt[t-1,1:dbeta]%*%Beta))
					   # cat("\nte=",(Xt[t-1,1:dbeta]%*%Beta))
             } #end for t
            }
           }
 				#   if(model=="Normal"){
 				#   if(is.null(Xt)){
  			#	  for(t in 2:(n+1)){ #begin for t
  			#	   att[t-1] <- StaPar[1]*at[t-1]
    		#		 btt[t-1] <- StaPar[1]*bt[t-1]
    		#		 at[t] <- att[t-1]+(1/2)
				#	   bt[t] <- btt[t-1]+(Yt[t-1]^2)/2
				#	  }
				#	  }else{
				#	   for(t in 2:(n+1)){ #begin for t
  			#	     att[t-1] <- StaPar[1]*at[t-1]
    		#		   btt[t-1] <- StaPar[1]*bt[t-1]*exp(-(Xt[t-1,1:dbeta]%*%Beta))
    		#		   at[t] <- att[t-1]+(1/2)
				#	     bt[t] <- StaPar[1]*bt[t-1]+((Yt[t-1]^2)/2)*exp((Xt[t-1,1:dbeta]%*%Beta))
  			#	     btmu[t] <-(StaPar[1]*bt[t-1]+((Yt[t-1]^2)/2)*exp((Xt[t-1,1:dbeta]%*%Beta)))*exp(-(Xt[t-1,1:dbeta]%*%Beta))
	
				#	  }
 				 #  } #end for t
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
  				       btmu[t] <-(StaPar[1]*bt[t-1]+((Yt[t-1]^2)/2)*exp((Xt[t-1,1:dbeta]%*%Beta)))*exp(-(Xt[t-1,1:dbeta]%*%Beta))
              }else{
					     bt[t] <- StaPar[1]*bt[t-1]+(((Yt[t-1]-(Zt[t-1,1:dteta]%*%Teta))^2)/2)*exp((Xt[t-1,1:dbeta]%*%Beta))
 				       btmu[t] <-(StaPar[1]*bt[t-1]+(((Yt[t-1]-(Zt[t-1,1:dteta]%*%Teta))^2)/2)*exp((Xt[t-1,1:dbeta]%*%Beta)))*exp(-(Xt[t-1,1:dbeta]%*%Beta))

                    }
              } #end for t     
					  }
            } 
					  
 				   
 				   
   	   #    if(model=="Laplace"){
   	    #   if(is.null(Xt)){
      	 #   for(t in 2:(n+1)){   #begin for t
      	  #   att[t-1] <- StaPar[1]*at[t-1]
    			#	 btt[t-1] <- StaPar[1]*bt[t-1]
    			#	 at[t] <- att[t-1]+(1)
					 #  bt[t] <- btt[t-1]+sqrt(2)*abs(Yt[t-1])
					#  } #end for t
					#  }else{
					#   for(t in 2:(n+1)){   #begin for t
      	   #   att[t-1] <- StaPar[1]*at[t-1]
    			#	  btt[t-1] <- StaPar[1]*bt[t-1]*exp(-(Xt[t-1,1:dbeta]%*%Beta))
    			#	  at[t] <- att[t-1]+(1)
					 #   bt[t] <- StaPar[1]*bt[t-1]+(sqrt(2)*abs(Yt[t-1]))*exp((Xt[t-1,1:dbeta]%*%Beta))
				  #    btmu[t] <-(StaPar[1]*bt[t-1]+(sqrt(2)*abs(Yt[t-1]))*exp((Xt[t-1,1:dbeta]%*%Beta)))*exp(-(Xt[t-1,1:dbeta]%*%Beta))

				#	   } #end for t
			#		  }
 			#	   }
 				   
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
					    btmu[t] <-(StaPar[1]*bt[t-1]+(sqrt(2)*abs(Yt[t-1]))*exp((Xt[t-1,1:dbeta]%*%Beta)))*exp(-(Xt[t-1,1:dbeta]%*%Beta))
					    }else{bt[t] <- StaPar[1]*bt[t-1]+(sqrt(2)*abs(Yt[t-1]-(Zt[t-1,1:dteta]%*%Teta)))*exp((Xt[t-1,1:dbeta]%*%Beta))
                    btmu[t] <-(StaPar[1]*bt[t-1]+(sqrt(2)*abs(Yt[t-1]-(Zt[t-1,1:dteta]%*%Teta)))*exp((Xt[t-1,1:dbeta]%*%Beta)))*exp(-(Xt[t-1,1:dbeta]%*%Beta))

                   }
					   } #end for t
					  }
 				   }

 				   
      #     if(model=="GED"){
       #    if(is.null(Xt)){
     	#		  at[1]    <- 1/((1-StaPar[1])*StaPar[2])
   		#	    bt[1]    <- StaPar[1]/(StaPar[1]*StaPar[2]+abs(StaPar[1]-1)*(StaPar[2]^2))
			 #   	for(t in 2:(n+1)){  #begin for t
       #      att[t-1] <- StaPar[1]*at[t-1]
    		#		 btt[t-1] <- StaPar[1]*bt[t-1]
	   	#			 psi   <- ((gamma(3/StaPar[2]))/gamma(1/StaPar[2]))^(StaPar[2]/2)
    	#			 at[t] <- att[t-1]+(1/StaPar[2])
			#		   bt[t] <- btt[t-1]+((abs(Yt[t-1]))^StaPar[2])*psi
			#		  }  #end for t
			#		  }else{
     	#		  at[1]    <- 1/((1-StaPar[1])*StaPar[2])
   		#	    bt[1]    <- StaPar[1]/(StaPar[1]*StaPar[2]+abs(StaPar[1]-1)*(StaPar[2]^2))
			 #   	for(t in 2:(n+1)){  #begin for t
        #     att[t-1] <- StaPar[1]*at[t-1]
    		#		 btt[t-1] <- StaPar[1]*bt[t-1]*exp(-(Xt[t-1,1:dbeta]%*%Beta))
	   		#		 psi   <- ((gamma(3/StaPar[2]))/gamma(1/StaPar[2]))^(StaPar[2]/2)
    		#		 at[t] <- att[t-1]+(1/StaPar[2])
				#	   bt[t] <- StaPar[1]*bt[t-1]+(((abs(Yt[t-1]))^StaPar[2])*psi)*exp((Xt[t-1,1:dbeta]%*%Beta))
		     #    btmu[t] <-(StaPar[1]*bt[t-1]+(((abs(Yt[t-1]))^StaPar[2])*psi)*exp((Xt[t-1,1:dbeta]%*%Beta)))*exp(-(Xt[t-1,1:dbeta]%*%Beta))

					#  }  #end for t

         #  }
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
		         btmu[t] <-(StaPar[1]*bt[t-1]+(((abs(Yt[t-1]))^StaPar[2])*psi)*exp((Xt[t-1,1:dbeta]%*%Beta)))*exp(-(Xt[t-1,1:dbeta]%*%Beta))
					   }else{
              bt[t] <- StaPar[1]*bt[t-1]+(((abs(Yt[t-1]-(Zt[t-1,1:dteta]%*%Teta)))^StaPar[2])*psi)*exp((Xt[t-1,1:dbeta]%*%Beta))
   		         btmu[t] <-(StaPar[1]*bt[t-1]+(((abs(Yt[t-1]-(Zt[t-1,1:dteta]%*%Teta)))^StaPar[2])*psi)*exp((Xt[t-1,1:dbeta]%*%Beta)))*exp(-(Xt[t-1,1:dbeta]%*%Beta))
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
             btmu[t] <-(StaPar[1]*bt[t-1]+(Yt[t-1])*exp((Xt[t-1,1:dbeta]%*%Beta)))*exp(-(Xt[t-1,1:dbeta]%*%Beta))

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
             btmu[t] <-(StaPar[1]*bt[t-1]+(Yt[t-1]^StaPar[3])**exp((Xt[t-1,1:dbeta]%*%Beta)))*exp(-(Xt[t-1,1:dbeta]%*%Beta))

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
             btmu[t] <-(StaPar[1]*bt[t-1]+(Yt[t-1]^StaPar[2])*exp(Xt[t-1,1:dbeta]%*%Beta))*exp(-(Xt[t-1,1:dbeta]%*%Beta))

					  }  #end for t

					  }
 				   } # end for

      if(distl=="FILTER"){
        mab<- matrix(0,2,n+1)
        if(is.null(Xt)){
          mab[1,]=at
          mab[2,]=t(bt)
        }else{
        mab[1,]=at
        mab[2,]=t(btmu)
               }
        if(splot==TRUE){ #Begin Plot
          ###################################################################### 
           n1=length(at)
           ytm=matrix(0,n1,1)
           ytm[,1]=mab[1,]/mab[2,]
  #         ytm[,2]=Perc1
   #        ytm[,3]=Perc2
      #     minyt=min(ytm[2:(n1-1),1])
      #     maxyt=quantile(ytm[2:(n1-1),1],c(0.9))
           #at = seq(as.Date("1999/12/02"),as.Date("2000/12/21"),"days") 
           ##d=as.Date(dat,format="%Y-%m-%d")
           #d=at
           #dat=seq(d[1], d[length(d)], by="month")
           at=1:n1
          # dev.new()
           plot(at,ytm[,1],xlab="t",ylab=expression(paste(hat(mu)[t])),type='l',
           lty=c(1),lwd=c(2),col=c("black"),main="Estimates of States")
       #    par(new=TRUE)
        #   plot(at,ytm[,2],xlab="t",ylab=expression(paste(lambda[t])),type='s',
         #  ylim=c(minyt,maxyt),lty=c(2),lwd=c(2),col=c("blue"))
          # par(new=TRUE)
           #plot(at,ytm[,3],xlab="t",ylab=expression(paste(lambda[t])),type='s',
           #ylim=c(minyt,maxyt),lty=c(2),lwd=c(2),col=c("blue"))
           #axis.Date(1,at=dat,labels=dat, las = 2) 
           #axis(2, at =seq(min(Yt), max(Yt), 1), las = 1, tck = +0.01,cex.axis=0.7)
           #mtext(side = 2, "Yt", line = 3.0)  #Y-axis label
           legend("topright", c("Filtered Mean"),lty=c(1),lwd=c(2),
            col=c("black"), cex=0.68, bty="n", pt.cex = 1)
         } #End Plot   
        return(mab)
      }else{
        mab2<- matrix(0,2,n)
        mab2[1,]=att
        mab2[2,]=btt
       if(splot==TRUE){ #Begin Plot
          ###################################################################### 
           n1=length(att)
           ytm=matrix(0,n1,1)
           ytm[,1]=att/btt
  #         ytm[,2]=Perc1
   #        ytm[,3]=Perc2
           minyt=min(ytm[2:(n1-1),1])
           maxyt=max(ytm[2:(n1-1),1])
           #at = seq(as.Date("1999/12/02"),as.Date("2000/12/21"),"days") 
           ##d=as.Date(dat,format="%Y-%m-%d")
           #d=at
           #dat=seq(d[1], d[length(d)], by="month")
           at=1:n1
          #  dev.new()
           plot(at,ytm[,1],xlab="t",ylab=expression(paste(hat(mu)[t])),type='l',
           ylim=c(minyt,maxyt),lty=c(1),lwd=c(2),col=c("black"),main="Estimates of States")
       #    par(new=TRUE)
        #   plot(at,ytm[,2],xlab="t",ylab=expression(paste(lambda[t])),type='s',
         #  ylim=c(minyt,maxyt),lty=c(2),lwd=c(2),col=c("blue"))
          # par(new=TRUE)
           #plot(at,ytm[,3],xlab="t",ylab=expression(paste(lambda[t])),type='s',
           #ylim=c(minyt,maxyt),lty=c(2),lwd=c(2),col=c("blue"))
           #axis.Date(1,at=dat,labels=dat, las = 2) 
           #axis(2, at =seq(min(Yt), max(Yt), 1), las = 1, tck = +0.01,cex.axis=0.7)
           #mtext(side = 2, "Yt", line = 3.0)  #Y-axis label
           legend("topright", c("Predicted Mean"),lty=c(1),lwd=c(2),
            col=c("black"), cex=0.68, bty="n", pt.cex = 1)
            
         } #End Plot 
        return(mab2)
      }
}#End Time Series Models


if(model=="SRGamma" || model=="SRWeibull"){                # Begin SR
if(model=="SRGamma"){model1="Gamma"}
if(model=="SRWeibull"){model1="Weibull"}
if (is.null(Event)==FALSE)stop("Bad input Event for this model")
if (is.null(Break)==FALSE)stop("Bad input Break for this model")
if (amp==TRUE)stop("Bad input for amp for this model")
if(is.null(Zt)==FALSE)stop("Bad input for Zt for this model")
set.seed(1000)
################################################################################
##
## Weibull Example 
##
################################################################################

     if (a0 <= 0) stop("Bad input value for a0")
     if (b0 <= 0) stop("Bad input value for b0")
     if (is.null(Yt))stop("Bad input Yt")
     if (is.vector(Yt)==FALSE)stop("Bad input for Yt")
     if (distl!="PRED" && distl!="FILTER")stop("Bad input for distl")
     if (is.vector(Xt))stop("Bad input for Xt. Put as a matrix.")
     if (is.null(StaPar))stop("Bad input for StaPar")
     if (is.data.frame(StaPar))stop("Bad input for StaPar")
     if (is.vector(StaPar)==FALSE)stop("Bad input for StaPar")
     if (model1!="Gamma"&&model1!="Weibull")stop("Bad input for model")
     if (sum(length(which(is.na(Yt))))>0)stop("Bad input Yt")
     if(is.null(Xt)==FALSE){if (sum(length(which(is.na(Xt))))>0)stop("Bad input Xt")}
     
     if(StaPar[1]==0)stop("Bad input for the static parameter w: value outside the parameter space.")
    # if(StaPar[1]==0){StaPar[1]=StaPar[1]+0.001}
     #print(b0)

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

      if(distl=="FILTER"){
        mab<- matrix(0,2,n+1)
        mab[1,]=at
        mab[2,]=btmu
        #return(mab)
       if(splot==TRUE){ #Begin Plot
          ###################################################################### 
           n1=length(mab[1,])
           ytm=matrix(0,n1,1)
           ytm[2:(n1),1]=mab[1,2:(n1)]/mab[2,2:(n1)]
  #         ytm[,2]=Perc1
   #        ytm[,3]=Perc2
           minyt=min(ytm[2:(n1-1),1])
           maxyt=max(ytm[2:(n1-1),1])
           #at = seq(as.Date("1999/12/02"),as.Date("2000/12/21"),"days") 
           ##d=as.Date(dat,format="%Y-%m-%d")
           #d=at
           #dat=seq(d[1], d[length(d)], by="month")
           aat=1:n1
          #  dev.new()
           plot(aat[2:(n1)],ytm[2:(n1),1],xlab="t",ylim=c(minyt,maxyt),ylab=expression(paste(hat(lambda)[t])),type='l',
           lty=c(1),lwd=c(2),col=c("black"),main="Estimates of States")
       #    par(new=TRUE)
        #   plot(at,ytm[,2],xlab="t",ylab=expression(paste(lambda[t])),type='s',
         #  ylim=c(minyt,maxyt),lty=c(2),lwd=c(2),col=c("blue"))
          # par(new=TRUE)
           #plot(at,ytm[,3],xlab="t",ylab=expression(paste(lambda[t])),type='s',
           #ylim=c(minyt,maxyt),lty=c(2),lwd=c(2),col=c("blue"))
           #axis.Date(1,at=dat,labels=dat, las = 2) 
           #axis(2, at =seq(min(Yt), max(Yt), 1), las = 1, tck = +0.01,cex.axis=0.7)
           #mtext(side = 2, "Yt", line = 3.0)  #Y-axis label
           legend("topright", c("Filtered Mean"),lty=c(1),lwd=c(2),
            col=c("black"), cex=0.68, bty="n", pt.cex = 1)
         } #End Plot  
        return(mab)
      }else{
        mab2<- matrix(0,2,n)
        mab2[1,]=att
        mab2[2,]=btt
       if(splot==TRUE){ #Begin Plot
          ###################################################################### 
           n1=length(att)
           ytm=matrix(0,n1,1)
           ytm[,1]=att/btt
  #         ytm[,2]=Perc1
   #        ytm[,3]=Perc2
           minyt=min(ytm[3:(n1-1),1])
           maxyt=max(ytm[3:(n1-1),1])
           #at = seq(as.Date("1999/12/02"),as.Date("2000/12/21"),"days") 
           ##d=as.Date(dat,format="%Y-%m-%d")
           #d=at
           #dat=seq(d[1], d[length(d)], by="month")
           at=1:n1
          #  dev.new()
           plot(at,ytm[,1],xlab="t",ylab=expression(paste(hat(mu)[t])),type='l',
           lty=c(1),lwd=c(2),col=c("black"),main="Estimates of States")
       #    par(new=TRUE)
        #   plot(at,ytm[,2],xlab="t",ylab=expression(paste(lambda[t])),type='s',
         #  ylim=c(minyt,maxyt),lty=c(2),lwd=c(2),col=c("blue"))
          # par(new=TRUE)
           #plot(at,ytm[,3],xlab="t",ylab=expression(paste(lambda[t])),type='s',
           #ylim=c(minyt,maxyt),lty=c(2),lwd=c(2),col=c("blue"))
           #axis.Date(1,at=dat,labels=dat, las = 2) 
           #axis(2, at =seq(min(Yt), max(Yt), 1), las = 1, tck = +0.01,cex.axis=0.7)
           #mtext(side = 2, "Yt", line = 3.0)  #Y-axis label
           legend("topright", c("Predicted Mean"),lty=c(1),lwd=c(2),
            col=c("black"), cex=0.68, bty="n", pt.cex = 1)
         } #End Plot
        return(mab2)
      }

} # SR Models


if(model=="PEM"){                                   #Begin PEM/PH Model
#amp=FALSE
if(is.null(Zt)==FALSE)stop("Bad input for Zt for this model")
################################################################################ 
#
## PEM Example 
##
################################################################################ 
set.seed(1000)
    if (a0 <= 0) stop("Bad input value for a0")
     if (b0 <= 0) stop("Bad input value for b0")
     if (is.null(Yt))stop("Bad input Yt")
     if (is.vector(Yt)==FALSE)stop("Bad input for Yt")
     if (distl!="PRED" && distl!="FILTER")stop("Bad input for distl")
     if (is.vector(Xt))stop("Bad input for Xt. Put as a matrix.")
     if (is.null(StaPar))stop("Bad input for StaPar")
     if (is.data.frame(StaPar))stop("Bad input for StaPar")
     if (is.vector(StaPar)==FALSE)stop("Bad input for StaPar")
     if (model!="PEM")stop("Bad input for model")
     if (sum(length(which(is.na(Yt))))>0)stop("Bad input Yt")
     if(is.null(Xt)==FALSE){if (sum(length(which(is.na(Xt))))>0)stop("Bad input Xt")}
     if (is.null(Event))stop("Bad input Event")
     if (is.null(Break))stop("Bad input Break")

     if(StaPar[1]==0){StaPar[1]=StaPar[1]+0.001}     
     if(StaPar[1]==0)("Bad input for the static parameter w: value outside the parameter space.")


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
			    
      if(distl=="FILTER"){
        mab<- matrix(0,2,n+1)
        mab[1,]=at
        mab[2,]=btmu
        if(splot==TRUE){ #Begin Plot
          ###################################################################### 
           n1=length(at)
           ytm=matrix(0,n1,1)
           ytm[,1]=at/btmu
  #         ytm[,2]=Perc1
   #        ytm[,3]=Perc2
            minyt=min(ytm[2:(n1-1),1])
            maxyt=max(ytm[2:(n1-1),1])
           #at = seq(as.Date("1999/12/02"),as.Date("2000/12/21"),"days") 
           ##d=as.Date(dat,format="%Y-%m-%d")
           #d=at
           #dat=seq(d[1], d[length(d)], by="month")
           at=1:n1
         #  dev.new()
           plot(at,ytm[,1],xlab="t",ylab=expression(paste(hat(mu)[t])),type='s',
           ylim=c(minyt,maxyt),lty=c(1),lwd=c(2),col=c("black"),main="Estimates of States")
       #    par(new=TRUE)
        #   plot(at,ytm[,2],xlab="t",ylab=expression(paste(lambda[t])),type='s',
         #  ylim=c(minyt,maxyt),lty=c(2),lwd=c(2),col=c("blue"))
          # par(new=TRUE)
           #plot(at,ytm[,3],xlab="t",ylab=expression(paste(lambda[t])),type='s',
           #ylim=c(minyt,maxyt),lty=c(2),lwd=c(2),col=c("blue"))
           #axis.Date(1,at=dat,labels=dat, las = 2) 
           #axis(2, at =seq(min(Yt), max(Yt), 1), las = 1, tck = +0.01,cex.axis=0.7)
           #mtext(side = 2, "Yt", line = 3.0)  #Y-axis label
           legend("topright", c("Filtered Mean"),lty=c(1),lwd=c(2),
            col=c("black"), cex=0.68, bty="n", pt.cex = 1)
         } #End Plot  
        mab
      }else{
        mab2<- matrix(0,2,n)
        mab2[1,]=att
        mab2[2,]=btt
        if(splot==TRUE){ #Begin Plot
          ###################################################################### 
           n1=length(at)
           ytm=matrix(0,n1,1)
           ytm[,1]=at/btmu
  #         ytm[,2]=Perc1
   #        ytm[,3]=Perc2
            minyt=min(ytm[2:(n1-1),1])
            maxyt=max(ytm[2:(n1-1),1])
           #at = seq(as.Date("1999/12/02"),as.Date("2000/12/21"),"days") 
           ##d=as.Date(dat,format="%Y-%m-%d")
           #d=at
           #dat=seq(d[1], d[length(d)], by="month")
           at=1:n1
        #   dev.new()
           plot(at,ytm[,1],xlab="t",ylab=expression(paste(hat(mu)[t])),type='s',
           ylim=c(minyt,maxyt),lty=c(1),lwd=c(2),col=c("black"),main="Estimates of States")
       #    par(new=TRUE)
        #   plot(at,ytm[,2],xlab="t",ylab=expression(paste(lambda[t])),type='s',
         #  ylim=c(minyt,maxyt),lty=c(2),lwd=c(2),col=c("blue"))
          # par(new=TRUE)
           #plot(at,ytm[,3],xlab="t",ylab=expression(paste(lambda[t])),type='s',
           #ylim=c(minyt,maxyt),lty=c(2),lwd=c(2),col=c("blue"))
           #axis.Date(1,at=dat,labels=dat, las = 2) 
           #axis(2, at =seq(min(Yt), max(Yt), 1), las = 1, tck = +0.01,cex.axis=0.7)
           #mtext(side = 2, "Yt", line = 3.0)  #Y-axis label
           legend("topright", c("Predicted Mean"),lty=c(1),lwd=c(2),
            col=c("black"), cex=0.68, bty="n", pt.cex = 1)
            
            ############################################################################
            ##
            ## GRAFICO SOMBREADO
            ##
            ############################################################################
            #seq1=seq(1,n1)
            #seq2=seq(n1,1)
            #seq3=array(0,c(n1))
            #plot.ts(log(ytmm[2:(n1),1]-0.00001), ylab=expression(log(y[i])),
            #xlab="Number of Failures",ylim=c(-2,10))
            #polygon(c(seq1, seq2),c(log(icddsup[1:n1]-0.00001),
            #rev(log(icddinf[1:n1]-0.00001))),col="light grey",border="light grey")
            #lines(log(ytmm[2:(n1),1]-0.00001))
            #lines(log(ytmm[2:(n1),2]-0.00001),lty = c("dashed"),lwd=c(2))
            #lines(log(sbe2[2:(n1)]-0.00001),lty = c("dotted"),lwd=c(2))

         } #End Plot
         mab2
         } 
       }#End PEM/PH Model
} #end funcfor
##########################################################

