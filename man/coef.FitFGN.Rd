\name{coef.FitFGN}
\alias{coef.FitFGN}
\title{ Display estimated parameters from FitFGN }
\description{
Method function to display fitted parameters, their standard
errors and Z-ratio for FGN models fit with FitFGN.
}
\usage{
\method{coef}{FitFGN}(object, ...)
}
\arguments{
  \item{object}{ \code{obj} the output from \code{FitFGN} }
  \item{...}{optional parameters}
}

\value{
A matrix is returned. The columns of the matrix are labeled
MLE, sd and Z-ratio.  The rows labels indicate the AR coefficients
which were estimated followed by mu, the estimate of mean.
}

\references{
McLeod, A.I., Yu, Hao, Krougly, Zinovi L.  (2007).
Algorithms for Linear Time Series Analysis,
Journal of Statistical Software.
}

\author{ A.I. McLeod }

\examples{
data(NileMin)
out<-FitFGN(NileMin)
coef(out)

}
\keyword{ ts }
