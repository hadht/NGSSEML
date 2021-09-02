\name{PlotF}
\alias{PlotF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Plot Function 
}
\description{
  The function PlotF gives graphs with smoothed/filtered estimates of the 
  latent states. 
   
}

\usage{
PlotF(formula, data,na.action="na.omit",pz=NULL,nBreaks=NULL,
plotYt=TRUE,axisxdate=NULL,transf=1,model="Poisson",posts,Proc="Smooth",
Type="Marg",distl="PRED",a0=0.01,b0=0.01,ci=0.95,startdate=NULL,enddate=NULL,
Freq=NULL,...)


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
     \item{transf}{
       This argument allows the user to apply a transformation (exponentiation) in the estimates 
       of the latent states. For example, the inverse transformation, i. e., transf = -1. 
       The default value is 1. Optional argument.  
     }
  
  \item{model}{
       the chosen model for the observations. The options are: Poisson, Normal, Gamma,
Weibull, Generalized Gamma, Laplace, GED and PEM models.
} 
\item{posts}{
     A sample or an estimate of the static parameters.
} 

      \item{plotYt}{
             If true, the time series Yt is inserted in the plot. 
              The default value is TRUE. Optional argument.
     }
      \item{axisxdate}{
      a date vector for the x-axis can be specified in this function. 
              The default value is NULL. Optional argument.      
     }
 
  \item{Proc}{
   the latent states distribution to be returned. There are 2 options: 
     the smoothed ("Smooth") and filtering ("Filter") distributions.
 } 
  \item{Type}{
    the chosen distribution of the lantent states. There are 2 options: 
     conditional ("Cond") on the static parameters and marginal ("Marg"). The default 
     is "Marg". 

} 
 \item{distl}{
     the chosen distribution of the lantent states in the filtering procedure. There are 2 options: the one-step ahead ("PRED") and filtering ("Filter") distributions. The default is "PRED". 
}
\item{a0}{
    the shape parameter of the initial Gamma distribution. Optional argument.
Default: a0=0.01.
}  \item{b0}{
       the scale parameter of the initial Gamma distribution. Optional argument.
     Default: b0=0.01.
     }
\item{ci}{
     the nominal level of confidence interval for the parameters. Optional argument.
     Default: ci=0.95.
} 
\item{startdate}{
     If the argument axisxdate is not NULL, it is necessary to specify a start date. Optional argument.
} 
\item{enddate}{
     If the argument axisxdate is not NULL, it is necessary to specify an end date. Optional argument.
} 
\item{Freq}{
     If the argument axisxdateis not NULL, it is necessary to specify a frequency of the data. Optional argument.
} 
\item{...}{
     Other arguments if it is necessary.
} 

}
\details{
%%  ~~ If necessary, more details than the description above ~~
Typical usages are
\preformatted{PlotF(YYtm~Trend+CosAnnual+SinAnnual+CosSemiAnnual+SinSemiAnnual,
data=data.frame(Ytm,Xtm),model="Poisson",StaPar=estopt,axisxdate=x,Proc="Smooth",
Type="Cond",distl="FILTER",a0=0.01,b0=0.01,ci=0.95,posts=estopt,
startdate="1970/01/01",enddate="1983/12/31",Freq="months",...)}

}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{graph}{This function returns an graph with smoothed or filtered estimates of the latent states. }
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
\code{\link{SmoothingF}}
\code{\link{ngssm.bayes}}
\code{\link{ngssm.mle}}

}
\examples{

###############################################################################
## Petro data:
library(NGSSEML)
## Inputs: 
data(Return_data)
Ytm = Return_data$Rt
Ytm = Ytm[1:200]
Date = Return_data$Date
Xtm = NULL
Ztm = NULL
model = "GED"
LabelParTheta = c("W", "nu")
StaPar=c(0.9, 1)
p = length(StaPar)
nn = length(Ytm)
a0 = 0.01
b0  = 0.01
pointss = 4   ### points
nsamplex = 50 ## Sampling posterior
ci = 0.95       # Cred. level
fitbayes=ngssm.bayes(Ytm~1, data = data.frame(Ytm), model = model, pz = NULL,
StaPar = StaPar, a0 = a0, b0 = b0, prw = c(1, 1), prnu = c(0.01, 0.01), 
ci = ci, pointss = pointss, nsamplex = nsamplex, postplot = FALSE, 
contourplot = FALSE, LabelParTheta = LabelParTheta, verbose = TRUE)
#postaux = fitbayes$samplepost[,]
posts = fitbayes$samplepost

##Smoothing
##PlotF function

#PlotF(Ytm~1, data = data.frame(Ytm), model = model, pz = NULL, plotYt = FALSE,
#transf = -0.5, Proc = "Smooth", Type = "Marg", distl = "PRED", a0 = a0, b0 = b0,
#ci = ci, posts = posts, startdate = NULL, enddate = NULL, Freq = "days", 
#typeline = 'l', col = c("black", "blue", "lightgrey"), xlab = "t", ylab = 
#expression(paste(hat(sigma)[t])), xlim = NULL, ylim = c(0.02, 0.10), 
#lty = c(1, 2,1), lwd = c(2, 2, 2), cex = 0.68)
###############################################################################
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Dynamic model}
\keyword{ NGSSM}
\keyword{ Exact likelihood}
\keyword{ States and observations forecasting}
\keyword{ States filtering}% __ONLY ONE__ keyword per line
