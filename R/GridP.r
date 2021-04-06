################################################################################
################################################################################
##
##
#'@noRd

GridP<- function(Yt, Event, nT = NULL){
    if (is.null(Event))stop("Bad input Event")
    if (is.null(Yt))stop("Bad input Yt")
    if (is.vector(Yt)==FALSE)stop("Bad input for Yt")
  ordem<- order(Yt)
  Yt<- Yt[ordem]
  Event<- Event[ordem]
  temposfalha.distintos<- unique(Yt[Event == 1])
  if(is.null(nT)){
    nT<- length(temposfalha.distintos)
  }
  m<- length(temposfalha.distintos)
  if(nT > m){
    a<- c(0, unique(Yt[Event == 1]))
    a[length(a)]<- Inf
  }else{
    b<- min(m, nT)
    k1<- trunc(m / b)
    r<- m - b*k1
    k2<- k1 + 1
    idf1<- seq(k1, (b-r)*k1, k1)
    idf2<- sort(seq(m, max(idf1), -k2))
    idf<- unique(c(idf1, idf2))
    a<- c(0, temposfalha.distintos[idf])
    a[length(a)]<- Inf
  }
  Break=a
  return(Break)
}
################################################################################
################################################################################

