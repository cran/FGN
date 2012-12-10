\name{Boot}
\alias{Boot}
\title{Generic Bootstrap Function  }
\description{
Generic function to bootstrap a fitted model.
}
\usage{
Boot(obj, R=1, ...)
}
\arguments{
  \item{obj}{fitted object }
  \item{R}{number of bootstrap replicates }
  \item{...}{optional arguments }
}

\value{
Parametric bootstrap simulation
}

\references{
McLeod, A.I., Yu, Hao, Krougly, Zinovi L.  (2007).
Algorithms for Linear Time Series Analysis,
Journal of Statistical Software.
}

\author{ A.I. McLeod }


\seealso{ \code{\link{Boot.FitFGN}} }
\examples{
\dontrun{
data(NileMin)
out<-FitFGN(NileMin)
Boot(out, R=3)
}
}
\keyword{ ts }
