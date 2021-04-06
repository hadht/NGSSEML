################################################################################
##
##
## NGSSM: BAYESIAN ESTIMATION
##
##
################################################################################
##
##
#'@noRd
gridfunction<-function(npoints,linf,lsup){
npar=length(linf)
xgrid=matrix(0,npoints,npar)
for(i in 1:npar){
xgrid[,i]=seq(linf[i], lsup[i], length.out = npoints)
}
grid.l=data.frame(xgrid)
gridoutput=make.surface.grid(grid.l)
return(gridoutput[,])
}

################################################################################
