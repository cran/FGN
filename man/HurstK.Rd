\name{HurstK}
\alias{HurstK}
\title{ Hurst K Coefficient }
\description{
  The Hurst K provides a non-parametric estimate for the Hurst H coefficient
}
\usage{
HurstK(z)
}
\arguments{
  \item{z}{  time series vector }
}
\details{
There are many alternative non-parametric estimators for H.
Some of the popular ones are discussed in Hipel and McLeod (2005).
}
\value{
an estimate of H 
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

\seealso{  \code{\link{FitFGN}} }
\examples{
# the Hurst coefficient for NID series is 0.5
z<-rnorm(1000)
HurstK(z)
#Hurst K for Nile Minima
data(NileMin)
HurstK(NileMin)
}
\keyword{ ts }
\keyword{ nonparametric }
