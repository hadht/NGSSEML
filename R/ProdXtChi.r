################################################################################
##
#'@noRd
ProdXtChi<-function(StaPar, Yt, Break, Event, Xt) {
  if (is.null(Event))
    stop("Bad input Event")
  if (is.null(Break))
    stop("Bad input Break")
  if (is.null(Yt))
    stop("Bad input Yt")
  if (is.vector(Yt) == FALSE)
    stop("Bad input for Yt")
  if (is.null(Xt)==FALSE){if(is.matrix(Xt)==FALSE){Xt=as.matrix(Xt)}}
  if (is.vector(Xt))
    stop("Bad input for Xt. Put as a matrix.")
  if (is.null(StaPar))
    stop("Bad input for StaPar")
  if (is.data.frame(StaPar))
    stop("Bad input for StaPar")
  n <- length(Yt)
  if (is.null(Xt) == FALSE) {
    if (is.null(dim(Xt))) {
      dbeta = dim(t(Xt))[1]
      dStaPar = length(StaPar)
      Beta = matrix(StaPar[(dStaPar - dbeta + 1):(dStaPar)],
                    dbeta, 1)
    }
    else {
      dbeta = dim(Xt)[2]
      dStaPar = length(StaPar)
      Beta = matrix(StaPar[(dStaPar - dbeta + 1):(dStaPar)],
                    dbeta, 1)
    }
  }
  ksi <- 0
  nT <- length(Break) - 1
  N <- length(Yt)
  auxiliar <- matrix(0, ncol = nT, nrow = N)
  for (j in 1:nT) {
    for (i in 1:N) {
      if (Yt[i] > Break[j] & Yt[i] <= Break[j + 1]) {
        auxiliar[i, j] <- Xt[i, 1:dbeta] %*% Beta
      }
    }
    ksi[j] <- sum(auxiliar[, j])
  }
  cco=ksi
  return(cco)
}
##########################################################
