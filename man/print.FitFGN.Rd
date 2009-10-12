\name{print.FitFGN}
\alias{print.FitFGN}
\title{ Print Method for "FitFGN" Object }

\description{
A terse summary is given.
}

\usage{
\method{print}{FitFGN}(x, ...)
}

\arguments{
  \item{x}{ object of class "FitFGN" }
  \item{...}{ optional arguments }
}

\value{
A terse summary is displayed
}

\references{ 
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
FitFGN(NileMin)
}

\keyword{ ts }
