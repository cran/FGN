\name{FGNAcf}
\alias{FGNAcf}
\title{ Autocorrelation of FGN }
\description{
The FGN time series is an example of a time series exhibiting long-range dependence
and characterized by the fact that its autocorrelation function exhibits hyperbolic
decay rather than exponential decay found in stationary ARMA time series.
The FGN and other alternatives are discussed in Hipel and McLeod (2005).
}
\usage{FGNAcf(k, H)}
\arguments{
  \item{k}{ lag or lags - may be vector }
  \item{H}{ Hurst parameter }
}
\value{
value of the autocorrelation at lag(s) k
}
\references{
Hipel, K.W. and McLeod, A.I., (2005).
Time Series Modelling of Water Resources and Environmental Systems.
Electronic reprint of our book orginally published in 1994.
\url{http://www.stats.uwo.ca/faculty/aim/1994Book/}.

McLeod, A.I., Yu, Hao, Krougly, Zinovi L.  (2007).
Algorithms for Linear Time Series Analysis,
Journal of Statistical Software.

}

\author{ A.I. McLeod}

\note{ The parameter H should be in (0,1). An error message
is given if it is not. }
\seealso{ \code{\link{FGNLL}}, \code{\link{acf}} }
\examples{
#compute the acf at lags 0,1,...,10 when H=0.7
FGNAcf(0:10, 0.7) 
}
\keyword{ ts }
