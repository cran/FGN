\name{NileFlowCMS}
\alias{NileFlowCMS}
\docType{data}
\title{ Annual flow of Nile River at Aswan, 1871-1945}
\description{
This is average annual flow of the Nile River below the Aswan Dam.
The units are CMS (cubic meters per second).
}
\usage{data(NileFlowCMS)}
\format{
  The format is:
 Time-Series [1:75] from 1870 to 1944: 3958 3370 3485 3438 3702 ...
}  

\source{
  Hipel, K.W. and McLeod, A.I., (2005).
  Time Series Modelling of Water Resources and Environmental Systems.
   Electronic reprint of our book orginally published in 1994.
\url{http://www.stats.uwo.ca/faculty/aim/1994Book/}.
}

\examples{
#Plot the time series
data(NileFlowCMS)
ts.plot(NileFlowCMS)

#Hurst K estimate
HurstK(NileFlowCMS)

}

\keyword{datasets}
