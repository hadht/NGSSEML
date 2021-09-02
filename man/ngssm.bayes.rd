\name{ngssm.bayes}
\alias{ngssm.bayes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Bayesian estimation of the non-Gaussian state space models with
exact marginal likelihood
}
\description{
  The function performs the Bayesian estimation for 
the static parameters of the model.
}
\usage{
ngssm.bayes(formula,data,na.action="na.omit",pz=NULL,nBreaks=NULL,
model="Poisson",StaPar=NULL,amp=FALSE,a0=0.01,b0=0.01,prw=c(1,1),
prnu=NULL,prchi=NULL,prmu=NULL,prbetamu=NULL,prbetasigma=NULL,lower=NULL,
upper=NULL,ci=0.95,pointss=10,nsamplex=1000,mcmc=NULL,postplot=FALSE,contourplot=FALSE,
LabelParTheta=NULL,verbose=FALSE)

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
  \item{prw}{
        a numeric vector of length 2, indicating the hyperparameters of the 
       Beta prior distribution for the parameter w. Optional argument. The
default value is c(1,1), which constitutes an uninformative prior for common data sets.
}  

  \item{prnu}{
      a numeric vector of length 2, indicating the hyperparameters of the 
       Gamma prior distribution for the shape parameter nu. Optional argument. 
         }  

  \item{prchi}{
       a numeric vector of length 2, indicating the hyperparameters of the 
       Gamma prior distribution for the shape parameter chi. Optional argument. 
}  
  \item{prmu}{
       a numeric vector of length 2, indicating mean and standard deviation for the 
       Gaussian prior distribution for the parameter mu. Optional argument.
        This prior can be used in Normal, Laplace and
GED time series models.

}  
  \item{prbetamu}{
   a numeric vector of length p, indicating mean for the 
       Gaussian prior distribution for the parameter beta, the regression coefficients. Optional argument. 

       
       }  
  \item{prbetasigma}{
      a numeric matrix of order p by p, indicating variance-covariance matrix of the 
       Gaussian prior distribution for the parameter beta, the regression coefficients. Optional argument.

}   
 \item{lower}{
     an lower bound for the static parameters (StaPar) in the density support argument 
     of the ARMS function (MCMC). Optional argument.
} 
 \item{upper}{
     an upper bound for the static parameters (StaPar) in the density support argument 
     of the ARMS function (MCMC).  Optional argument.
} 
 \item{ci}{
     the nominal level of credibility interval for the parameters.
     Default: ci=0.95. Optional argument.
} 
  \item{pointss}{
     the number of points/parts/breaks that the specified interval of 
     the static parameters is partitioned. Default: pointss=10.
}
  \item{nsamplex}{
     the number of samples of the posterior distribution of 
     the static parameters, obtained by numerical integration. If this 
     posterior is computed via ARMS, nsamplex is the number of samples from the posterior 
     distribution of the static parameters, assuming a burn-in period of 1000. Default: samples=3000.
}
  \item{mcmc}{
     If true, the ARMS method is used to sample the marginal posterior distribution of the static 
     parameters. If false, a grid of points is used to sample the marginal posterior distribution 
     of the static parameters. Otherwise, if the mcmc argument is NULL, a suitable chose is done.
     Default: mcmc=NULL. Optional argument.
}
 \item{postplot}{
     If true, a graph with the marginal posterior distribution of 
     the static parameters is provided. Optional argument.
}
\item{contourplot}{
     If true, a countour plot of the posterior distribution of 
     the static parameters is provided. Optional argument.
}
\item{LabelParTheta}{
     If not NULL, the static parameters are called by the specified 
     label. The default value is NULL. Optional argument. 
}
\item{verbose}{
     A logical variable that gives the user the output of the model fit in the console. 
     Default: TRUE. Optional argument.
}
  
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Typical usages are
\preformatted{ngssm.bayes(Ytm~Trend+CosAnnual+SinAnnual+CosSemiAnnual+SinSemiAnnual,
data=data.frame(Ytm,Xtm),model=model,StaPar=c(0.8,-0.8,0.01,0.01,0.01,0.01),
prw=c(1,1),prbetamu=rep(0,5),prbetasigma=diag(10, 5, 5),pointss=5,nsamplex=1000)
}

}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{[[1]]}{This function returns the output of Bayesian estimation 
  for the static parameters.}
  \item{[[2]]}{This function returns posterior samples of the static parameters 
  using multinomial sampling scheme.}

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
This function provides summaries of the posterior distribution of the static parameters of the specified model. 
In an exact way, the posterior is built to make inferences for the static parameters, and samples of it are drawn 
using multinomial sampling. If the dimensionality of static parameters and the break number of the grid are high, 
there are many points to evaluate the posterior distribution and, hence, an MCMC method (ARMS) is used to sample 
the posterior distribution of the static parameters. Furthermore, it is necessary to specify the limits of 
the parametric space of the model for the ARMS function in the arguments 'lower' and 'upper'. 

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{SmoothingF}}
\code{\link{ngssm.mle}}

}
\examples{

################################################################################ 
##
## PEM Example: the GTE data 
##
################################################################################ 
library(NGSSEML)
data(gte_data)
Ytm = gte_data$V1
## Event: failure, 1.
Event = gte_data$V2  
Breakm = NGSSEML:::GridP(Ytm, Event, nT = NULL)
Xtm = NULL
Ztm = NULL
model = "PEM"
amp = FALSE
##LabelParTheta = c("w")
StaPar = c(0.9)
p = length(StaPar)
nn = length(Ytm)
a0 = 0.01
b0 = 0.01
## points
pointss = 4 
## Posterior sample
nsamplex = 100 
ci = 0.95
alpha = 1-ci
#Bayesian fit
fitbayes = ngssm.bayes(Ytm~Event, data = data.frame(Ytm, Event), model = model, 
pz = NULL, amp = amp, a0 = a0, b0 = b0, prw = c(1, 1), prnu = NULL, 
prchi = NULL, prmu= NULL, prbetamu = NULL, prbetasigma = NULL, ci = ci, 
pointss = pointss, nsamplex = nsamplex, postplot = FALSE, contourplot = FALSE)
################################################################################
}
%% Add one or more standard keywords, see file 'KEYWORDS' in the
%% R documentation directory.
\keyword{State space model}
\keyword{NGSSM}
\keyword{Exact likelihood}
\keyword{Bayesian estimation}
\keyword{States and observations forecasting}
\keyword{States filtering}% __ONLY ONE__ keyword per line
\keyword{States smoothing}% __ONLY ONE__ keyword per line
