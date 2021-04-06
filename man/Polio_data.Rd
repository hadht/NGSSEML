\name{Polio_data}
\alias{Polio_data}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
The Polio Data

}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
The data consist of
monthly counts of poliomyelitis cases in the USA from the year 1970 to 1983.
}
\usage{data(Polio_data)}
\format{
  A data frame with 168 observations on the following 8 variables.
  
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
The covariates are the deterministic trend centered at 73 and divided by 1000, 
annual and semiannual cosine and annual and semiannual sine. 
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
Centers for Disease Control, USA.
}
\references{
%%  ~~ possibly secondary sources and usages ~~
Zeger, S.L. (1988). A regression model for time series
of counts. Biometrika 75, 621-29.
}
\examples{
data(Polio_data)
}
\keyword{datasets}
