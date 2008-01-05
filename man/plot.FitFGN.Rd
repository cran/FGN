\name{plot.FitFGN}
\alias{plot.FitFGN}
\title{ Plot Method for "FitFGN" Object }
\description{
Diagnostic plots of the residual autocorrelations and Ljung-Box test.
}
\usage{
\method{plot}{FitFGN}(x, maxLag=30, ...)
}
\arguments{
  \item{x}{ object of class "FitFGN" }
  \item{maxLag}{ maximum lag in residual acf plot }
  \item{...}{ optional arguments }
}
\value{
No value is returned. A plots are produced as side-effect.
The plot is a two-panel disply showing the residual autocorrelations
and the p-values for the Ljung-Box test.
}

\details{
The top plot shows the residual autocorrelations and their 5\% signficance
limits.
The bottom plot shows the p-values of the Ljung-Box test for various lags.
}

\references{
Ljung, G.M., Box, G.E.P. (1978).
On a Measure of Lack of Fit in Time Series Models.
Biometrika 65, 297-303.

McLeod, A.I., Yu, Hao, Krougly, Zinovi L.  (2007).
Algorithms for Linear Time Series Analysis,
Journal of Statistical Software.
}

\author{ A.I. McLeod}

\seealso{ 
\code{\link{summary.FitFGN}}, 
\code{\link{FitFGN}} 
 }
\examples{
data(NileMin)
obj<-FitFGN(NileMin, c(1,2,6,7))
plot(obj)
}
\keyword{ ts }



