\name{PacfToAR}
\alias{PacfToAR}
\title{Transform from PACF Parameters to AR Coefficients }
\description{
Transforms AR partical autocorrelation function (PACF) 
parameters to AR coefficients based on the Durbin-Levinson recursion.
}
\usage{
PacfToAR(zeta)
}
\arguments{
  \item{zeta}{ vector of AR PACF parameters }
}
\details{
See Mcleod and Zhang (2006)
}
\value{
Vector of AR coefficients
}
\references{ 
McLeod, A.I. and Zhang, Y. (2006).  
Partial autocorrelation parameterization for subset autoregression.  
Journal of Time Series Analysis, 27, 599-612.
}
\author{ A.I. McLeod and Y. Zhang}

\seealso{ 
\code{\link{InvertibleQ}}, 
\code{\link{PacfToAR}}
}
\examples{
somePACF<-c(0.5,0.6,0.7,0.8,-0.9,-0.8)
someAR<-PacfToAR(somePACF)
test<-ARToPacf(someAR)
#this should be very small
sum(abs(test-somePACF))
}
\keyword{ts }
