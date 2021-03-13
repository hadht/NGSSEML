################################################################################
################################################################################
##
##
#'@noRd
NumFail<- function(StaPar,Yt,Event,Break,Xt){
    if (is.null(Event))stop("Bad input Event")
    if (is.null(Break))stop("Bad input Break")
    if (is.null(Yt))stop("Bad input Yt")
    if (is.vector(Yt)==FALSE)stop("Bad input for Yt")
    if(is.null(Xt)==FALSE){if(is.matrix(Xt)==FALSE){Xt=as.matrix(Xt)}}
    if (is.vector(Xt))stop("Bad input for Xt. Put as a matrix.")
    if (is.null(StaPar))stop("Bad input for StaPar")
    if (is.data.frame(StaPar))stop("Bad input for StaPar")

  ni<- NULL
  nT<- length(Break) - 1
  for(j in 1:nT){
    ni[j]<- length(which(Yt > Break[j] & Yt <= Break[j+1] & Event == 1))
  }
  nf=ni
  return(nf)
}
################################################################################
################################################################################
