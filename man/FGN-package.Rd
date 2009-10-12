\name{FGN-package}
\alias{FGN-package}
\docType{package}

\title{
Estimate H parameter by MLE
}

\description{
Compute FGN's rank correlation and various trend tests.
}

\details{
\tabular{ll}{
Package: \tab FGN\cr
Type: \tab Package\cr
Version: \tab 1.2\cr
Date: \tab 2009-10-12\cr
License: \tab GPL (>= 2)\cr
LazyLoad: \tab yes\cr
}

This package provides a comprehensive approach to fitting  FGN.

}


\author{
A. I. McLeod

Maintainer: aimcleod@uwo.ca
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

\keyword{ts}

\keyword{package}

\seealso{
\code{\link{HurstK}}, 
\code{\link{FitFGN}}, 
\code{\link{FitRegressionFGN}},
\code{\link{SimulateFGN}},
\code{\link{print.FitFGN}}, 
\code{\link{summary.FitFGN}},
\code{\link{predict.FitFGN}},
\code{\link{plot.FitFGN}},
\code{\link{residuals.FitFGN}}
}		

\examples{
#Compare HurstK and MLE for H
#Hurst K for Nile Minima
data(NileMin)
HurstK(NileMin)
out<-FitFGN(NileMin)
summary(out)
plot(out)
coef(out)
}
