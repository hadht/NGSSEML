\name{SmoothingF}
\alias{SmoothingF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Smoothing Distribution (Procedure) of the Latent States 
}
\description{
  The function SmoothingF gives an exact sample 
  of the posterior distribution of the latent states condiotinal on the static parameters 
  or marginal. 
}
\usage{
 SmoothingF(formula,data,na.action="na.omit",pz=NULL,nBreaks=NULL,
 model="Poisson",StaPar=NULL,Type="Cond",a0=0.01,b0=0.01,
 amp=FALSE,samples=1,ci=0.95,splot=FALSE)
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
  \item{Type}{
     the chosen distribution of the lantent states. There are 2 options: 
     conditional on the static parameters and marginal ("Marg"). The default 
     is conditional ("Cond").
} 

\item{a0}{
    the shape parameter of the initial Gamma distribution. Optional argument.
DDefault: a0=0.01.
}  \item{b0}{
       the scale parameter of the initial Gamma distribution. Optional argument.
     Default: b0=0.01.
     }
 
\item{amp}{
     the interval width is taken in account in the estimation of parameter w 
     which controls the loss of information over time, only for the PEM. For more 
     details see Santos et al. (2017). Default: FALSE. Optional argument.
}
\item{samples}{
     the number of samples drawn from the joint posterior distribution of the 
     latent states,
     given a point of the static parameters (StaPar). Optional argument. Default: samples = 1.   
} 
\item{ci}{
     the nominal level of confidence interval for the parameters. Optional argument.
     Default: ci=0.95.
} 
\item{splot}{
     Create a plot with the point and interval estimates of the states. Optional argument.
} 

}
\details{
%%  ~~ If necessary, more details than the description above ~~
Typical usages are
\preformatted{SmoothingF(Ytm~Trend+CosAnnual+SinAnnual+CosSemiAnnual+SinSemiAnnual,
data=data.frame(Ytm,Xtm),model="Poisson",Type="Cond",a0=0.01,b0=0.01,samples=1,ci=0.95)}

}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{mdata}{This function returns an exact sample of the join distribution of the states. 
  If the number of samples is greater than 1, some summaries of the state samples are returned. }
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
The model options are the Poisson, Normal, Laplace, GED, Gamma, Weibull and Generalized Gamma models.
'Zt' are the explanatory time series only for the Normal, Laplace and GED volatility 
     models.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{FilteringF}}
\code{\link{ngssm.mle}}
\code{\link{ngssm.bayes}}

}
\examples{
##PEM
##GTE Data
data(gte_data)
Ytm = gte_data$V1
Event = gte_data$V2   
Breakm = NGSSEML:::GridP(Ytm, Event, nT = NULL)
Xtm = NULL
Ztm = NULL
model = "PEM"
amp = FALSE
LabelParTheta = c("w")
StaPar = c(0.73)
p = length(StaPar)
nn = length(Breakm)
a0 = 0.01
b0 = 0.1
p=length(StaPar)
pointss = 4    ### points
nsamplex = 50 ## Multinomial sampling posterior
ci = 0.95
alpha = 1-ci
#Bayesian fit:
fitbayes = ngssm.bayes(Ytm~Event, data = data.frame(Ytm, Event), model = model,
pz = NULL, StaPar = StaPar, amp = amp, a0 = a0, b0 = b0, prw = c(1,1), 
prnu = NULL, prchi = NULL, prmu = NULL, prbetamu = NULL, prbetasigma = NULL, 
ci = ci, pointss = pointss, nsamplex = nsamplex, postplot = FALSE, 
contourplot = FALSE, LabelParTheta = LabelParTheta, verbose = TRUE)
posts = fitbayes$samplepost
#Smoothing
set.seed(1000)
fits = SmoothingF(Ytm~Event, data = data.frame(Ytm, Event), model = model, 
pz = NULL, StaPar = posts, Type = "Marg", a0 = a0, b0 = b0, ci = ci, 
samples = 1, splot = FALSE)
###############################################################################
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Dynamic model}
\keyword{ NGSSM}
\keyword{ Exact likelihood}
\keyword{ States and observations forecasting}
\keyword{ States filtering}% __ONLY ONE__ keyword per line
