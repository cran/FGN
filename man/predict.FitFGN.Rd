\name{predict.FitFGN}
\alias{predict.FitFGN}
\title{ Forecasts from a fitted FGN model }
\description{
The exact finite-sample minimum mean square error forecasts are computed
using the Trench algorithm.
}
\usage{
\method{predict}{FitFGN}(object, n.ahead = 1, ...)
}
\arguments{
  \item{object}{ "FitFGN" object produced by FitFGN }
  \item{n.ahead}{ forecasts are done for lead times 1,...,n.ahead }
  \item{\dots}{ optional arguments, are ignored }
}
\value{
A list with components
  \item{Forecasts }{matrix with m+1 rows and maxLead columns with the forecasts}
  \item{SDForecasts }{matrix with m+1 rows and maxLead columns with the sd of the forecasts}
}


\references{ 
McLeod, A.I., Yu, Hao, Krougly, Zinovi L.  (2007).
Algorithms for Linear Time Series Analysis,
Journal of Statistical Software.
}

\author{ A.I. McLeod }


\seealso{ 
\code{\link{FitFGN}},
\code{\link[LTSA]{TrenchForecast}} 
\code{\link[LTSA]{PredictionVariance}} 
\code{\link{predict.Arima}} 
}

\examples{
data(NileMin)
out<-FitFGN(NileMin)
predict(out, n.ahead=15)
}
\keyword{ ts }
