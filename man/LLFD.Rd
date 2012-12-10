\name{LLFD}
\alias{LLFD}
\title{
Concentrated Loglikelihood Function for d
}
\description{
The concentrated loglikelihood, that is, the loglikelihood function
maximized over the innovation variance parameter, is computed.
}
\usage{
LLFD(d, z)
}

\arguments{
  \item{d}{
fractional difference parameter
}
  \item{z}{
time series
}
}

\value{
the value of the loglikelihood
}

\author{
A. I. McLeod
}

\seealso{
\code{\link{GetFitFD}}
}
\examples{
#compute loglikelihood for NileFlowCMS with 
#  H=0.9 and with d=H-0.5=0.4
data(NileFlowCMS)
z<-NileFlowCMS
z<-z-mean(z)
LLFGN(0.9, z)
LLFD(0.4, z)
}

\keyword{ ts }
