\name{acvfPLA}
\alias{acvfPLA}

\title{
autocovariance function for the PLA model
}
\description{
The autocovariance function is computed for the power-law autocorrelation time series model.
}
\usage{
acvfPLA(alpha, maxlag)
}

\arguments{
  \item{alpha}{
decay parameter
}
  \item{maxlag}{
maximum lag
}
}

\value{
vector of maxlag+1 containing the autocovariances at lags 0, 1, ..., maxlag.
}

\author{
A. I. McLeod and Justin Veenstra
}
\note{
alpha=1 corresponds to white noise
}

\seealso{
\code{\link{LLPLA}}
}

\examples{
acvfPLA(0.4, 10)
}
\keyword{ ts }
