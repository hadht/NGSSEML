################################################################################
##
##
## NGSSM: MAXIMUM LIKELIHOOD ESTIMATION
##
##
################################################################################
##
##

ngssm.mle<-function(formula, data,
                    na.action="na.omit",pz=NULL,nBreaks=NULL,
                    model="Poisson",StaPar=NULL,amp=FALSE,
                    a0=0.01,b0=0.01,ci=0.95,LabelParTheta=NULL,verbose=FALSE,
                    method="BFGS",hessian=TRUE,control=list(maxit = 30000, temp = 2000, trace = FALSE,REPORT = 500)){
  #NA
  if(na.action=="na.omit"){na.omit(data)}
  call <- match.call()
  oldoptions <-options(warn=-1)
  on.exit(options(oldoptions)) 
  
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
  #  Break=NULL
  
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
  #options(warn=-1)
  #oldoptions <-options(warn=-1)
  #on.exit(options(oldoptions)) 
  
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
  #Xt[,i]=get(names(dataf)[i+2])
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
  # if(is.null(pz)!=TRUE){
  #nnnd=dim(dataf)[1]
  #ppd=dim(dataf)[2]-1-pz
  #Xtdd=matrix(0,nnnd,ppd)
  #if(pz>=1){
  #for(i in 1:ppd){
  #Xt[,i]=get(names(dataf)[i+1])
  #Xtdd[,i]=dataf[[names(dataf)[i+1]]]  
  #}
  #}
  #Zt=matrix(0,nnnd,pz)
  #for(j in 1:pz){
  #Zt[,j]=get(names(dataf)[j+ppd+1])
  #Ztdd[,j]=dataf[[names(dataf)[j+ppd+1]]]
  
  #}
  #}
  #}
  #Yt<-Ytdd
  #Xt<-Xtdd
  #Zt<-Ztdd
  #detach(dataf)
  #print(Yt)
  #print(Xt)
  #print(Zt)
  #print(Event)
  #print(Break)
  
  #############################################################################
  #############################################################################
  #############################################################################
  
  #if(!require("akima"))stop("Warning: akima package is required!")
  #if(!require("fields"))stop("Warning: fields package is required!")
  #if(!require("mvtnorm"))stop("Warning: mvtnorm package is required!")
  #if(!require("NGSSEML"))stop("Warning: akima package is required!")
  if(is.null(Xt)==FALSE){if(is.matrix(Xt)==FALSE){Xt=as.matrix(Xt)}}
  if(is.null(Zt)==FALSE){if(is.matrix(Zt)==FALSE){Zt=as.matrix(Xt)}}
  if(is.null(Xt)==FALSE){if(dim(Xt)[2]>18)stop("Many covariates!!!")}
  if(is.null(Zt)==FALSE){if(dim(Zt)[2]>18)stop("Many covariates!!!")}
  
  if (model=="Poisson" || model=="Normal" || model=="Laplace" || model=="GED"||   #TS
      model=="Gamma" || model=="GGamma" || model=="Weibull"){
    #if (is.null(Event)==FALSE)stop("Bad input Event for this model")
    #if (is.null(Break)==FALSE)stop("Bad input Break for this model")
    if (amp==TRUE)stop("Bad input for amp for this model")
    
    lower=numeric()
    upper=numeric()
    
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
          pp=dim(Zt)[2]
          LabelParThetaaux=c("w","Delta1","Delta2","Delta3","Delta4","Delta5","Delta6","Delta7","Delta8","Delta9","Delta10",
                             "Delta11","Delta12","Delta13","Delta14","Delta15","Delta16","Delta17","Delta18","Delta19","Delta20")
          LabelParTheta=LabelParThetaaux[1:(1+pp)]
        }
        
        if(is.null(Xt)==FALSE && is.null(Zt)==TRUE){
          ppp=dim(Xt)[2]
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
          pp=dim(Zt)[2]
          LabelParThetaaux=c("w","Delta1","Delta2","Delta3","Delta4","Delta5","Delta6","Delta7","Delta8","Delta9","Delta10",
                             "Delta11","Delta12","Delta13","Delta14","Delta15","Delta16","Delta17","Delta18","Delta19","Delta20")
          LabelParTheta=LabelParThetaaux[1:(1+pp)]
        }
        
        if(is.null(Xt)==FALSE && is.null(Zt)==TRUE){
          ppp=dim(Xt)[2]
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
    
    #############################################################################################
    #print(length(LabelParTheta))
    #print(length(StaPar))
    if ((is.null(LabelParTheta)==FALSE)){
      if(is.null(StaPar)==FALSE){
        if(length(LabelParTheta)!=length(StaPar))stop("Bad input for StaPar and LabelParTheta for this model")
      }
    }
    # If default of StaPar is true, initialize StaPar.  
    #Begin Default StarPar
    if(is.null(StaPar)==FALSE){
      StaPar1=StaPar;
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
        StaPar[1]=((0.8));StaPar1[1]=log(-log(StaPar[1]));if(dxt>0){StaPar[2:(dxt+1)]=rep(0,dxt);};
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
        StaPar[1]=((0.9));StaPar1[1]=log(-log(StaPar[1]));if(dxt>0){StaPar[2:(dxt+1)]=rep(0,dxt);};if(dzt>0){StaPar[(dxt+1):(dxt+1+dzt)]=rep(0,dzt);};
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
        StaPar[1]=((0.9));StaPar1[1]=log(-log(StaPar[1]));if(dxt>0){StaPar[2:(dxt+1)]=rep(0,dxt);};if(dzt>0){StaPar[(dxt+1):(dxt+1+dzt)]=rep(0,dzt);};
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
        StaPar=numeric(p);
        StaPar[1]=0.9;StaPar1=StaPar;StaPar1[1]=log(-log(StaPar[1]));StaPar[2]=(1);StaPar1[2]=log(StaPar[2]);
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
        StaPar=numeric(p);
        StaPar[1]=0.9;StaPar1=StaPar;StaPar1[1]=log(-log(StaPar[1]));StaPar[2]=(1);StaPar1[2]=log(StaPar[2]);if(p>2){StaPar[3:(dxt+2)]=rep(0,dxt);};
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
        StaPar=numeric(p);
        StaPar[1]=0.9;StaPar1=StaPar;StaPar1[1]=log(-log(StaPar[1]));StaPar[2]=(1);StaPar1[2]=log(StaPar[2]);StaPar[3]=(1);StaPar1[3]=log(StaPar[3]);if(p>3){StaPar[4:(dxt+2)]=rep(0,dxt);};
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
        StaPar=numeric(p);
        StaPar[1]=0.9;StaPar1=StaPar;StaPar1[1]=log(-log(StaPar[1]));StaPar[2]=(1);StaPar1[2]=log(StaPar[2]);if(p>2){StaPar[3:(dxt+2)]=rep(0,dxt);};
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
    
    if (model=="Poisson" || model=="Gamma" || model=="GGamma" || model=="Weibull"){
      if(is.null(Zt)==FALSE)stop("Bad input for Zt for this model")
    }
    #print(StaPar1)
    if(verbose) cat("\n*****Non-Gaussian State Space Models with Exact Likelihood*****\n","\nNGSSEML Package:","MLE -",model,"\n")
    resultsopt=NA
    resultsopt=tryCatch(optim(StaPar1, hessian=hessian,LikeF2,
                              method=method,na.action=na.action,Yt=Yt,Xt=Xt,Zt=Zt,Break=Break,Event=Event,
                              a0=a0,b0=b0,model=model,
                              control = control), error = c)
    if(is.null(resultsopt$convergence)){print(resultsopt$convergence);stop("Convergence error! Bad inputs! Sorry!")}else{
      if(resultsopt$convergence!=0){print(resultsopt$convergence);stop("Convergence error! Bad inputs! Sorry!")}}
    estopt=resultsopt$par
    estopt
    p=length(estopt)
    MIFopt=100*diag(p)
    LI=LS=rep(0,p)
    if(hessian==TRUE){
      Hessianmatrixopt=-resultsopt$hessian
      Hessianmatrixopt
      MIFopt=-solve(Hessianmatrixopt)
      MIFopt[1,1]=MIFopt[1,1]*(((exp(-exp(estopt[1])))*(-exp(estopt[1])))^2)
      MIFopt
      estopt[1]=exp(-exp(estopt[1]))
     # print(estopt)
      LI=estopt-1*(qnorm((1+ci)/2))*sqrt(diag(MIFopt))
      LS=estopt+1*(qnorm((1+ci)/2))*sqrt(diag(MIFopt))
      #LI[1]=exp(-exp(estopt[1]+1*(qnorm((1+ci)/2))*sqrt((MIFopt[1,1]))))
      #LS[1]=exp(-exp(estopt[1]-1*(qnorm((1+ci)/2))*sqrt((MIFopt[1,1]))))
      if(LI[1]<0){LI[1]=0}
      if(LS[1]>1){LS[1]=1}
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
        if(LI[2]<0){LI[2]=0}
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
        if(LI[2]<0){LI[2]=0}
        if(LI[3]<0){LI[3]=0}
      }
    }
    
    #cat("\n*****Non-Gaussian State Space Models with Exact Likelihood*****\n","\nNGSSMEL Package:","MLE -",model,"\n")
    p=length(estopt)
    nn=length(Yt)
  #  estopt[1]=exp(-exp(estopt[1]))
   # print(estopt)
    if(hessian==TRUE){
      mfit<-matrix(c(estopt,sqrt(diag(MIFopt)),estopt/sqrt(diag(MIFopt)),2*(1-pnorm(abs(estopt/sqrt(diag(MIFopt))))),LI,LS),p,6)
      colnames(mfit)=c("Est","SE","z","P-value","Lower","Upper")
      rownames(mfit)=paste0(c('\u03b8'),1:p)
      if(is.null(LabelParTheta)==FALSE){rownames(mfit)=LabelParTheta}
      ngssm.list<-list(mfit,-resultsopt$value,ci*100,nn)
      names(ngssm.list)<-c("Maximum Likelihood Estimation","LogLik","Nom. Level(%)","n.obs")
      if(verbose) print (ngssm.list)
      ngssm.list<-list(mfit,-resultsopt$value,ci*100,nn,estopt,formula,data)
    }else{
      mfit<-matrix(c(estopt),p,1)
      colnames(mfit)=c("Est")
      rownames(mfit)=paste0(c('\u03b8'),1:p)
      if(is.null(LabelParTheta)==FALSE){rownames(mfit)=LabelParTheta}
      ngssm.list<-list(mfit,-resultsopt$value,ci*100,nn)
      names(ngssm.list)<-c("Maximum Likelihood Estimation","LogLik","Nom. Level(%)","n.obs")
      if(verbose) print (ngssm.list)
      ngssm.list<-list(mfit,-resultsopt$value,ci*100,nn,estopt,formula,data)
    }
    names(ngssm.list)<-c("Maximum Likelihood Estimation","LogLik","Nom. Level(%)","n.obs","coefficients","formula","data")
    #return(ngssm.list)
    obj <- list()
    fitfit<-list(model, formula, estopt)
    fit<-list(sys = sys.call(),estopt)
    names(fit)[2]="coefficients"
    obj$fit <-fit
    obj$model<-model
    obj$formula<-formula
    obj$pz<-pz
    obj$a0<-a0
    obj$b0<-b0
    obj$nBreaks<-nBreaks
    obj$ci<-ci
    obj$na.action<-na.action
    obj$method<-list("Maximum Likelihood Estimation",method)
    obj$convergence<-resultsopt$convergence
    obj$control<-control
    obj$var <- diag(MIFopt)
    estoptc<-estopt
    if(is.null(LabelParTheta)){names(estoptc)=paste0(c('\u03b8'),1:p)}else{names(estoptc)=LabelParTheta}
    obj$coefficients<-estopt
    obj$data<-data
    #print(estopt)
    MeanSmooth=SmoothingF(formula=formula,data=data,model=model,
                          a0=a0,b0=b0,amp=amp,samples=3000,ci=ci,splot=FALSE,StaPar=estopt)
    obj$fitted.values<-MeanSmooth$Mean # FilteringF function
    obj$y<-obj$fitted.values
    #names(obj$y)<-c("Smoothed estimates")
    pnn<-length(obj$fitted.values)
    obj$x<-1:pnn
    #names(obj$x)<-c("Order obs.")
    obj$summary<-ngssm.list #colocar a lista que criei de output
    class(obj) = "ngssm.mle"
    class(ngssm.list) = "ngssm.mle"
    ob<-ngssm.list[[1]]; 
    class(ob) = "ngssm.mle"
    if(verbose==TRUE) {obj<-obj}else{obj<-ob}
    return(obj) 
    
    
  }
  
  if(model=="SRGamma" || model=="SRWeibull"){                #SR
    if(model=="SRGamma"){model1="Gamma"}
    if(model=="SRWeibull"){model1="Weibull"}
    #if (is.null(Event)==FALSE)stop("Bad input Event for this model")
    #if (is.null(Break)==FALSE)stop("Bad input Break for this model")
    if (amp==TRUE)stop("Bad input for amp for this model")
    if(is.null(Zt)==FALSE)stop("Bad input for Zt for this model")
    lower=numeric()
    upper=numeric()
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
      StaPar1=StaPar
      StaPar1[1]=log(-log(StaPar[1]));
      if (model=="SRWeibull"| model=="SRGamma"){   #Begin GED/Gamma/Weibull 
        StaPar1[2]=log(StaPar[2]);
      };
    };
    if(is.null(StaPar)){
      if (model1=="Gamma"){     #Begin Gamma
        ##StaPar:
        if(is.null(Xt)){dxt=0}else{dxt=dim(Xt)[2];}
        if(is.null(Zt)){dzt=0}else{dzt=dim(Zt)[2];}
        p=(1+1+dxt+dzt);
        StaPar=numeric(p);
        StaPar[1]=0.9;StaPar1=StaPar;StaPar1[1]=log(-log(StaPar[1]));StaPar[2]=(1);StaPar1[2]=log(StaPar[2]);if(p>2){StaPar[3:(dxt+2)]=rep(0,dxt);};
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
      
      if (model1=="Weibull"){    #Begin Weibull
        ##StaPar:
        if(is.null(Xt)){dxt=0}else{dxt=dim(Xt)[2];}
        if(is.null(Zt)){dzt=0}else{dzt=dim(Zt)[2];}
        p=(1+1+dxt+dzt);
        StaPar=numeric(p);
        StaPar[1]=0.9;StaPar1=StaPar;StaPar1[1]=log(-log(StaPar[1]));StaPar[2]=(1);StaPar1[2]=log(StaPar[2]);if(p>2){StaPar[3:(dxt+2)]=rep(0,dxt);};
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
    
    #cat("\n*****Non-Gaussian State Space Models with Exact Likelihood*****\n","\nNGSSMEL Package:","MLE -",model,"\n")
    nn=length(Yt)
    resultsopt=NA
    resultsopt=tryCatch(optim(StaPar1, hessian=hessian,LikeF2,
                              #lower=lower,upper=upper,
                              method=method,na.action=na.action,Yt=Yt,Xt=Xt,Zt=Zt,Break=Break,Event=Event,
                              a0=a0,b0=b0,model=model,
                              control = control), error = c)
    if(is.null(resultsopt$convergence)){print(resultsopt$convergence);stop("Convergence error! Bad inputs! Sorry!")}else{
      if(resultsopt$convergence!=0){print(resultsopt$convergence);stop("Convergence error! Bad inputs! Sorry!")}}
    estopt=resultsopt$par
    estopt
    #p=length(estopt)
    #MIFopt=100*diag(p)
    #LI=LS=rep(0,p)
    if(hessian==TRUE){
      Hessianmatrixopt=-resultsopt$hessian
      MIFopt=(-solve(Hessianmatrixopt))
      MIFopt[1,1]=MIFopt[1,1]*(((exp(-exp(estopt[1])))*(-exp(estopt[1])))^2)
      MIFopt[2,2]=MIFopt[2,2]*(((exp(estopt[2])))^2)
      MIFopt
      estopt[1]=exp(-exp(estopt[1]))
      estopt[2]=exp((estopt[2]))
      LI=estopt-1*(qnorm((1+ci)/2))*sqrt(diag(MIFopt))
      LS=estopt+1*(qnorm((1+ci)/2))*sqrt(diag(MIFopt))
      #LI[1]=exp(-exp(estopt[1]+1*(qnorm((1+ci)/2))*sqrt((MIFopt[1,1]))))
      #LS[1]=exp(-exp(estopt[1]-1*(qnorm((1+ci)/2))*sqrt((MIFopt[1,1]))))
      if(LI[1]<0){LI[1]=0}
      if(LS[1]>1){LS[1]=1}
      if(LI[2]<0){LI[2]=0}
    }
    
    if(verbose) cat ("\n*****Non-Gaussian State Space Models with Exact Likelihood*****\n","\nNGSSEML Package:","MLE -",model,"\n")
    p=length(estopt)
    if(hessian==TRUE){
      mfit<-matrix(c(estopt,sqrt(diag(MIFopt)),estopt/sqrt(diag(MIFopt)),2*(1-pnorm(abs(estopt/sqrt(diag(MIFopt))))),LI,LS),p,6)
      colnames(mfit)=c("Est","SE","z","P-value","Lower","Upper")
      rownames(mfit)=paste0(c('\u03b8'),1:p)
      if(is.null(LabelParTheta)==FALSE){rownames(mfit)=LabelParTheta}
      ngssm.list<-list(mfit,-resultsopt$value,ci*100,nn)
      names(ngssm.list)<-c("Maximum Likelihood Estimation","LogLik","Nom. Level(%)","n.obs")
      if(verbose) print (ngssm.list)
      ngssm.list<-list(mfit,-resultsopt$value,ci*100,nn,estopt,formula,data)
    }else{
      mfit<-matrix(c(estopt),p,1)
      colnames(mfit)=c("Est")
      rownames(mfit)=paste0(c('\u03b8'),1:p)
      if(is.null(LabelParTheta)==FALSE){rownames(mfit)=LabelParTheta}
      ngssm.list<-list(mfit,-resultsopt$value,ci*100,nn)
      names(ngssm.list)<-c("Maximum Likelihood Estimation","LogLik","Nom. Level(%)","n.obs")
      if(verbose) print (ngssm.list)
      ngssm.list<-list(mfit,-resultsopt$value,ci*100,nn,estopt,formula,data)
    }
    names(ngssm.list)<-c("Maximum Likelihood Estimation","LogLik","Nom. Level(%)","n.obs","coefficients","formula","data")
    #return(ngssm.list)
    
    obj <- list()
    fitfit<-list(model, formula, estopt)
    fit<-list(sys = sys.call(),estopt)
    names(fit)[2]="coefficients"
    obj$fit <-fit
    obj$model<-model
    obj$formula<-formula
    obj$pz<-pz
    obj$a0<-a0
    obj$b0<-b0
    obj$nBreaks<-nBreaks
    obj$ci<-ci
    obj$na.action<-na.action
    obj$method<-list("Maximum Likelihood Estimation",method)
    obj$convergence<-resultsopt$convergence
    obj$control<-control
    obj$var <- diag(MIFopt)
    estoptc<-estopt
    if(is.null(LabelParTheta)){names(estoptc)=paste0(c('\u03b8'),1:p)}else{names(estoptc)=LabelParTheta}
    obj$coefficients<-estopt
    obj$data<-data
    MeanSmooth=SmoothingF(formula=formula,data=data,model=model,
                          a0=a0,b0=b0,amp=amp,samples=3000,ci=ci,splot=FALSE,StaPar=estopt)
    obj$fitted.values<-MeanSmooth$Mean # FilteringF function
    obj$y<-obj$fitted.values
    #names(obj$y)<-c("Smoothed estimates")
    pnn<-length(obj$fitted.values)
    obj$x<-1:pnn
    #names(obj$x)<-c("Order obs.")
    obj$summary<-ngssm.list #colocar a lista que criei de output
    class(obj) = "ngssm.mle"
    class(ngssm.list) = "ngssm.mle"
    ob<-ngssm.list[[1]]; 
    class(ob) = "ngssm.mle"
    if(verbose==TRUE) {obj<-obj}else{obj<-ob}
    return(obj) 
    
  }
  
  if(model=="PEM"){
    #############################################################################################
    if ((is.null(LabelParTheta)==FALSE)){
      if(is.null(StaPar)==FALSE){
        if(length(LabelParTheta)!=length(StaPar))stop("Bad input for LabelParTheta for this model")
      }
    }
    lower=numeric()
    upper=numeric()
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
    # If default of StaPar is true, initialize StaPar.
    #Begin Default StarPar
    if(is.null(StaPar)){
      if (model=="PEM"){ #Begin PEM
        ##StaPar:
        if(is.null(Xt)){dxt=0}else{dxt=dim(Xt)[2];}
        if(is.null(Zt)){dzt=0}else{dzt=0;}
        p=(1+dxt+0);
        StaPar=numeric(p);
        StaPar[1]=0.9;StaPar1=StaPar;StaPar1[1]=log(-log(StaPar[1]));if(dxt>0){StaPar[2:(dxt+1)]=rep(0,dxt);};
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
      } #End PEM
      
    }#End Default StarPar
    #############################################################################################
    #Begin Default StarPar
    if(is.null(StaPar)==FALSE){
      StaPar1=StaPar;
      StaPar1[1]=log(-log(StaPar[1]));
    };
    #PEM
    #amp=FALSE
    if(is.null(Zt)==FALSE)stop("Bad input for Zt for this model")
    
    
    if(verbose) cat ("\n*****Non-Gaussian State Space Models with Exact Likelihood*****\n","\nNGSSEML Package:","MLE -",model,"\n")
    nn=length(Yt)
    resultsopt=NA
    #print(StaPar1)
    #print(formula)
    #print(data)
    #print(na.action)
    #print(pz)
    #print(check.env)
    #print(amp)
    #print(a0)
    #print(b0)
    #print(nBreaks)
    
    resultsopt=tryCatch(optim(StaPar1, hessian=hessian,LikeF2,
                              method=method,na.action=na.action,Yt=Yt,Xt=Xt,Zt=Zt,Break=Break,Event=Event,
                              amp=amp,a0=a0,b0=b0,model=model,
                              control = control), error = c)
    if(is.null(resultsopt$convergence)){print(resultsopt$convergence);stop("Convergence error! Bad inputs! Sorry!")}else{
      if(resultsopt$convergence!=0){print(resultsopt$convergence);stop("Convergence error! Bad inputs! Sorry!")}}
    estopt=resultsopt$par
    estopt
    #p=length(estopt)
    #MIFopt=100*diag(p)
    #LI=LS=rep(0,p)
    if(hessian==TRUE){
      Hessianmatrixopt=-resultsopt$hessian
      Hessianmatrixopt
      MIFopt=(-solve(Hessianmatrixopt))
      MIFopt[1,1]=MIFopt[1,1]*(((exp(-exp(estopt[1])))*(-exp(estopt[1])))^2)
      MIFopt
      estopt[1]=exp(-exp(estopt[1]))
      LI=estopt-1*(qnorm((1+ci)/2))*sqrt(diag(MIFopt))
      LS=estopt+1*(qnorm((1+ci)/2))*sqrt(diag(MIFopt))
      #LI[1]=exp(-exp(estopt[1]+1*(qnorm((1+ci)/2))*sqrt((MIFopt[1,1]))))
      #LS[1]=exp(-exp(estopt[1]-1*(qnorm((1+ci)/2))*sqrt((MIFopt[1,1]))))
      if(LI[1]<0){LI[1]=0}
      if(LS[1]>1){LS[1]=1}
    }
    #cat("\n*****Non-Gaussian State Space Models with Exact Likelihood*****\n","\nNGSSMEL Package:","MLE -",model,"\n")
    p=length(estopt)
    if(hessian==TRUE){
      mfit<-matrix(c(estopt,sqrt(diag(MIFopt)),estopt/sqrt(diag(MIFopt)),2*(1-pnorm(abs(estopt/sqrt(diag(MIFopt))))),LI,LS),p,6)
      colnames(mfit)=c("Est","SE","z","P-value","Lower","Upper")
      rownames(mfit)=paste0(c('\u03b8'),1:p)
      if(is.null(LabelParTheta)==FALSE){rownames(mfit)=LabelParTheta}
      ngssm.list<-list(mfit,-resultsopt$value,ci*100,nn)
      names(ngssm.list)<-c("Maximum Likelihood Estimation","LogLik","Nom. Level(%)","n.obs")
      if(verbose) print (ngssm.list)
      ngssm.list<-list(mfit,-resultsopt$value,ci*100,nn,estopt)
    }else{
      mfit<-matrix(c(estopt),p,1)
      colnames(mfit)=c("Est")
      rownames(mfit)=paste0(c('\u03b8'),1:p)
      if(is.null(LabelParTheta)==FALSE){rownames(mfit)=LabelParTheta}
      ngssm.list<-list(mfit,-resultsopt$value,ci*100,nn)
      names(ngssm.list)<-c("Maximum Likelihood Estimation","LogLik","Nom. Level(%)","n.obs")
      if(verbose) print (ngssm.list)
      ngssm.list<-list(mfit,-resultsopt$value,ci*100,nn,estopt)
    }
    names(ngssm.list)<-c("Maximum Likelihood Estimation","LogLik","Nom. Level(%)","n.obs","coefficients")
    #return(ngssm.list)
    obj <- list()
    fitfit<-list(model, formula, estopt)
    fit<-list(sys = sys.call(),estopt)
    names(fit)[2]="coefficients"
    obj$fit <-fit
    obj$model<-model
    obj$formula<-formula
    obj$pz<-pz
    obj$na.action<-na.action
    obj$method<-list("Maximum Likelihood Estimation",method)
    obj$convergence<-resultsopt$convergence
    obj$control<-control
    obj$var <- diag(MIFopt)
    estoptc<-estopt
    if(is.null(LabelParTheta)){names(estoptc)=paste0(c('\u03b8'),1:p)}else{names(estoptc)=LabelParTheta}
    obj$coefficients<-estopt
    obj$data<-data
    #if(is.null(a0)){a0<-0.01}
    #(is.null(b0)){b0<-0.01}
    #if(is.null(ci)){a0<-0.95}
    #cat(a0)
    obj$a0<-a0
    obj$b0<-b0
    obj$nBreaks<-nBreaks
    obj$ci<-ci
    MeanSmooth=SmoothingF(formula=formula,data=data,model=model,
                          a0=a0,b0=b0,amp=amp,samples=1000,pz=pz,ci=ci,splot=FALSE,StaPar=estopt)
    names(MeanSmooth$Mean)<-"Smoothed estimates"
    obj$fitted.values<-MeanSmooth$Mean # FilteringF function
    obj$y<-obj$fitted.values
    #names(obj$y)<-c("Smoothed estimates")
    pnn<-length(obj$fitted.values)
    obj$x<-1:pnn
    #names(obj$x)<-c("Order obs.")
    #obj$summary<-list(cat("\n*****Non-Gaussian State Space Models with Exact Likelihood*****\n","\nNGSSEML Package:","MLE -",model,"\n"),ngssm.list) #colocar a lista que criei de output
    obj$summary<-ngssm.list #colocar a lista que criei de output
    class(obj) = "ngssm.mle"
    ob<-ngssm.list[[1]]; 
    class(ob) = "ngssm.mle"
    if(verbose==TRUE) {obj<-obj}else{obj<-ob}
    return(obj) 
    
  }
  
  #return(ngssm.list)
  
}#End ngssm.mle