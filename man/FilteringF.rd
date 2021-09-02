\name{FilteringF}
\alias{FilteringF}
%- alterar o nome para FilteringF
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Filtering and One-Step-Ahead Distributions of the Latent States
}
\description{
  The function FilteringF gives the shape and scale parameters
of the filtering and the one-step-ahead forecast distributions of 
the latent states. 
}
\usage{
FilteringF(formula,data,na.action="na.omit",pz=NULL,
nBreaks=NULL,model="Poisson",StaPar=NULL,a0=0.01,b0=0.01,amp=FALSE,
distl="PRED",splot=FALSE)
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
 \item{a0}{
     the shape parameter of the initial Gamma distribution. Optional argument.
Default: a0=0.01.
}  \item{b0}{
      the scale parameter of the initial Gamma distribution. Optional argument.
     Default: b0=0.01.
}  
\item{amp}{
     the interval width is taken in account in the estimation of parameter w 
     which controls the loss of information over time, only for the PEM. For more 
     details see Santos et al. (2017). Default: FALSE. Optional argument.
}
\item{distl}{
      the latent states distribution to be returned.
      }
\item{splot}{
     a plot with the point and interval estimates of the states is provided. Optional argument.
} 

}
\details{
 Typical usages are
\preformatted{FilteringF(Yt~1,data=data.frame(Yt),StaPar=Par,model="Poisson",
a0=0.01,b0=0.01,splot=TRUE)}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{att}{'att' is the shape parameter of the one-step-ahead forecast distribution of the states.}
  \item{btt}{'btt' is the scale parameter of the one-step-ahead forecast distribution of the states.}
  \item{at}{'at' is the shape parameter of the filtering distribution of the states. It is necessary to specify this option in the argument 'distl'.}
  \item{bt}{'bt' is the scale parameter of the filtering distribution of the states. It is necessary to specify this option in the argument 'distl'.}

%% ...
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
It is necessary to specify the argument 'distl' in order to obtain the filtering distribution of the states.
The model options are the Poisson, Normal, Laplace, GED, Gamma, Weibull and Generalized Gamma models.
'Zt' are the explanatory time series only for the Normal, Laplace and GED volatility 
     models.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{SmoothingF}}

}
\examples{
library(NGSSEML)
Yt = c(1,2,1,4,3)
Par = c(0.9) #w
predpar = FilteringF(Yt~1, data = data.frame(Yt), StaPar = Par, model = "Poisson",
a0 = 0.01, b0 = 0.01, splot = FALSE)

filpar = FilteringF(Yt~1, data = data.frame(Yt), StaPar = Par, model = "Poisson",
a0 = 0.01, b0 = 0.01, distl = "FILTER", splot = FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Dynamic model}
\keyword{ NGSSM}
\keyword{ Exact likelihood}
\keyword{ States and observations forecasting}
\keyword{ States filtering}% __ONLY ONE__ keyword per line
