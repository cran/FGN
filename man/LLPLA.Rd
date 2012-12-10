\name{LLPLA}
\alias{LLPLA}
\title{
Concentrated Loglikelihood Function for parameter alpha in the PLA model
}
\description{
The concentrated loglikelihood, that is, the loglikelihood function
maximized over the innovation variance parameter, is computed.
}
\usage{
LLPLA(alpha, z)
}
\arguments{
  \item{alpha}{
decay parameter
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
\code{\link{GetFitPLA}}
}

\examples{
#compute loglikelihood for NileFlowCMS with 
#  H=0.9 and with alpha=2-2*H=0.2
data(NileFlowCMS)
z<-NileFlowCMS
z<-z-mean(z)
LLFGN(0.9, z)
LLPLA(0.2, z)
}

\keyword{ ts }
