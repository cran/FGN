\name{summary.FitFGN}
\alias{summary.FitFGN}
\title{ Summary Method for "FitFGN" Object }
\description{
summary for "FitFGN" object.
}
\usage{
\method{summary}{FitFGN}(object, ...)
}
\arguments{
  \item{object}{ "FitFGN" object }
  \item{...}{ optional arguments }
}
\value{
A printed summary is given
}
\author{ A.I. McLeod }


\seealso{ 
\code{\link{print.FitFGN}}, 
\code{\link{FitFGN}}
}
\examples{
data(NileMin)
out<-FitFGN(NileMin)
summary(out)
}
\keyword{ ts }
