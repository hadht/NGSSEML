\name{ngssm.mle}
\alias{ngssm.mle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Maximum likelihood estimation of the non-Gaussian state space models with
exact marginal likelihood  
}
\description{
  The function performs the marginal likelihood estimation for
the static parameters of the model.
}
\usage{
ngssm.mle(formula, data,na.action="na.omit",pz=NULL,
nBreaks=NULL,model="Poisson",StaPar=NULL,amp=FALSE,a0=0.01,
b0=0.01,ci=0.95,LabelParTheta=NULL,verbose=FALSE,method="BFGS",hessian=TRUE,
control=list(maxit = 30000, temp = 2000, trace = FALSE,REPORT = 500))

}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{formula}{
    an object of class "formula" (or one that can be coerced to that class): 
    a symbolic description of the model to be fitted.  
} 
  \item{data}{
  a data frame containing the variables in the model. The variables are: 
     - the time series of interest Yt (first column of the data frame).
     the explanatory time series to be inserted in the model. 
      - Xt must be always specified as a matrix of order n by p (after Yt).
      - the explanatory time series to be inserted in the mean of volatility 
     model. Zt must be always specified as a matrix of order n by p (after Xt).
     - a censoring indicator of the event (a vector), only for the PEM. If the 
     model is the PEM, put the variable Event in the secon column of tha data frame 
  after Yt, and he explanatory time series after the variable Event.
   The value 1 indicates failure.
     

}
  \item{na.action}{
    a function which indicates what should happen when the data contain NAs. 
    The default is set by the na.action setting 
    of options, and is na.fail if that is unset. Optional argument.
}
  \item{pz}{
    the number of the explanatory time series to be inserted in the mean of volatility 
     model. Default: NULL. Optional argument.
} 
\item{nBreaks}{
    the number of breaks used to build a vector with the interval limits, 
    only for the PEM. Optional argument.
}

\item{model}{
       the chosen model for the observations. The options are: Poisson, Normal, Gamma,
Weibull, Generalized Gamma, Laplace, GED and PEM models.
} 
\item{StaPar}{
    a numeric vector of initial values for the static parameters. Optional argument.
}
\item{amp}{
     the interval width is taken in account in the estimation of parameter w 
     which controls the loss of information over time, only for the PEM. For more 
     details see Santos et al. (2017). Default: FALSE. Optional argument.
}
 \item{a0}{
the shape parameter of the initial Gamma distribution. Optional argument.
Default: a0=0.01.
} 
 \item{b0}{
     the scale parameter of the initial Gamma distribution. Optional argument.
     Default: b0=0.01.
}
 \item{ci}{
     the nominal level of confidence interval for the parameters.
     Default: ci=0.95. Optional argument.
} 
\item{LabelParTheta}{
     If not NULL, the static parameters are called by the specified 
     label. Optional argument.
}  
\item{verbose}{
     A logical variable that gives the user the output of the model fit in the console. 
     Default: TRUE. Optional argument.
}
\item{method}{
     A variable that allows choosing a  maximization algorithm of the optim function. 
     Default: TRUE. Optional argument.
} 
\item{hessian}{
     A logical variable that allows calculating the hessian matrix numerically. 
     Default: TRUE. Optional argument.
} 
\item{control}{
     A list of control in the optim function. 
     Default: list(maxit = 30000, temp = 2000, trace = FALSE,REPORT = 500). Optional argument.
} 
  
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Typical usages are
\preformatted{
fit=ngssm.mle(Ytm~Trend+CosAnnual+SinAnnual+CosSemiAnnual+SinSemiAnnual,
data=data1,model="Poisson",StaPar=c(0.8,-0.8,0.01,0.01,0.01,0.01),
a0=0.01,b0=0.01,ci=0.95)
}

}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{[[1]]}{the output of the model fit, presenting the maximum likelihood 
estimators, standard errors, Z statistics, and asymptotic confidence intervals 
of the model parameters.
}
}
\references{
%% ~put references to the literature/web site here ~
Gamerman, D., Santos, T. R., and Franco, G. C. (2013). A Non-Gaussian Family of State-Space 
Models with Exact Marginal Likelihood. Journal of Time Series Analysis, 34(6), 625-645.

Santos T. R., Gamerman, D., Franco, G. C. (2017). Reliability Analysis via Non-Gaussian 
State-Space Models. IEEE Transactions on Reliability, 66, 309-318.

}
\author{
  T. R. Santos
}
\note{
%%  ~~further notes~~
The function provides the MLE estimates for 
the static parameters of the specified model. The 
likelihood function is maximized using the 'optim' 
function and 'BFGS' 
method. 

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{FilteringF}}
\code{\link{SmoothingF}}
\code{\link{ngssm.bayes}}

}
\examples{
## PEM Example: the GTE data
## MLE estimation
library(NGSSEML)
data(gte_data)
Ytm = gte_data$V1
Xtm = NULL
Ztm = NULL
model = "PEM"
amp = FALSE
## Event: failure, 1.
Event = gte_data$V2        
Break = NGSSEML:::GridP(Ytm, Event, nT = NULL)
##LabelParTheta = c("w")
StaPar = c(0.73)
a0 = 0.01
b0 = 0.01
ci = 0.95
fit = ngssm.mle(formula=Ytm~Event, data = data.frame(Ytm,Event), model = model, 
nBreaks= NULL, amp = amp, a0 = a0, b0 = b0, ci = ci)
##########################################################
} 
%% Add one or more standard keywords, see file 'KEYWORDS' in the
%% R documentation directory.
\keyword{State space model}
\keyword{NGSSM}
\keyword{Exact likelihood}
\keyword{Classical estimation}
\keyword{States and observations forecasting}
\keyword{States filtering}% __ONLY ONE__ keyword per line
\keyword{States smoothing}% __ONLY ONE__ keyword per line
