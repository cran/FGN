\name{residuals.FitFGN}
\alias{residuals.FitFGN}
\title{ Extract Residuals from "FitFGN" Object }
\description{
Method function.
}
\usage{
\method{residuals}{FitFGN}(object, \dots)
}
\arguments{
  \item{object}{ object of class "FitFGN" }
  \item{\dots}{ optional arguments }
}
\value{
Vector of standardized prediction residuals
}
\author{ A.I. McLeod }

\seealso{ 
\code{\link{FitFGN}} 
}
\examples{
data(NileMin)
out<-FitFGN(NileMin)
qqnorm(resid(out))
}
\keyword{ ts }
