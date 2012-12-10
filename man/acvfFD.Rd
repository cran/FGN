\name{acvfFD}
\alias{acvfFD}

\title{
autocovariance function of fractionally-differenced white noise
}
\description{
The autocovariance function of fractionally differenced white noise is computed 
for lags 0, 1, ..., maxlag. 
}
\usage{
acvfFD(d, maxlag)
}
\arguments{
  \item{d}{
fractional difference parameter
}
  \item{maxlag}{
maximum lag
}
}

\value{
vector of length maxlag+1 containing the autocovariances
}

\author{
A. I. McLeod
}

\note{
White noise corresponds to d=0.
}

\seealso{
\code{\link{LLFD}}
}

\examples{
acvfFD(0.2, 10)
}

\keyword{ ts }
