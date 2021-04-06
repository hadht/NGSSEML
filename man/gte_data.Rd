\name{gte_data}
\alias{gte_data}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
Daily failure times of 125 telecommunication systems
installed by the GTE

}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
The data are daily failure times of 125 telecommunication systems, including 
their respective censoring 
indicator, installed by the GTE corporation in a pre-specified time period (Kim and Proschan 1991).
}
\usage{data(gte_data)}
\format{
A data frame with 125 rows and 2 variables.  
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
The first column of the object gte_data corresponds to
 the failure times and the second to the censoring indicator.
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
Kim, J. S. and Proschan, R. (1991). Piecewise exponential estimator of
survivor function. IEEE Transactions on Reliability, 40, 134 to 139.

}
\references{
%%  ~~ possibly secondary sources and usages ~~
Kim, J. S. and Proschan, R. (1991). Piecewise exponential estimator of
survivor function. IEEE Transactions on Reliability, 40, 134 to 139.
}
\examples{
data(gte_data)
}
\keyword{datasets}
