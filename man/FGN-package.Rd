\name{FGN-package}
\alias{FGN-package}
\docType{package}

\title{
Fractional Gaussian Noise, estimation and simulaton
}

\description{
FGN (Fractional Gaussian Noise) model fitting, simulation, bootstrapping, 
forecasting. Mininum Nile River flow, annual, 663 values, 622 AD to 1284 AD.
Annual Nile river flow at Aswan, 1871-1945 from original source.
 
}

\details{
\tabular{ll}{
Package: \tab FGN\cr
Type: \tab Package\cr
Version: \tab 1.5\cr
Date: \tab 2011-02-11\cr
License: \tab GPL (>= 2)\cr
LazyLoad: \tab yes\cr
LazyData: \tab yes\cr
}

This package provides a comprehensive approach to fitting  FGN.

}


\author{
A. I. McLeod, Hao Yu and Zinovi Krougly.

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
