\name{ARToPacf}
\alias{ARToPacf}
\title{Reparametrize AR Coefficients In Terms of PACF}
\description{
 Transform AR parameter coefficients into partial autocorrelation function (PACF).
}
\usage{
ARToPacf(phi)
}

\arguments{
  \item{phi}{vector of AR parameter coefficients }
}
\details{
For details see McLeod and Zhang (2006).
}
\value{
Vector of length(phi) containing the parameters in the transformed 
PACF domain
}
\references{ 
McLeod, A.I. and Zhang, Y. (2006).  
Partial autocorrelation parameterization for subset autoregression.  
Journal of Time Series Analysis, 27, 599-612.
}

\author{ A.I. McLeod and Y. Zhang}

\section{Warning}{No check for invertibility is done for maximum computational efficiency
since this function is used extensively in the numerical optimization of
the AR loglikelihood function in FitAR.
Use InvertibleQ to test for invertible AR coefficients. 
}

\seealso{ 
\code{\link{InvertibleQ}}, 
\code{\link{PacfToAR}}
}
\examples{
somePACF<-c(0.5,0.6,0.7,0.8,-0.9,-0.8)
#PacfToAR() transforms PACF to AR parameter coefficients. 
someAR<-PacfToAR(somePACF)
test<-ARToPacf(someAR)
#This should be very small
sum(abs(test-somePACF))
}
\keyword{ ts }
