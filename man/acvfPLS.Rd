\name{acvfPLS}
\alias{acvfPLS}

\title{
autocovariance function for PLS model
}
\description{
The autocovariance function is computed for the PLS time series model.
}
\usage{
acvfPLS(alpha, maxlag)
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
acvfPLS(0.4, 10)
}

\keyword{ ts }

