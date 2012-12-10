\name{sdfPLS}
\alias{sdfPLS}

\title{
Spectral density function for PLS
}
\description{
Computes the spectral density function for the PLS model with parameter a
at the Fourier frequencies, 2*Pi*j/n, j=1,...,[n/2],
where n is the length of the time series.
}

\usage{
sdfPLS(p, n)
}

\arguments{
  \item{p}{
PLS parameter
}
  \item{n}{
length of time series
}
}
\details{
The details of the implementation are discussed in the accompanying vignette.
The parameter p should be in the interval (0, 2) and the length of the
series n should be greater than 2.
}
\value{
a vector of length [n/2] of the spectral density values.
}

\author{
A. I. McLeod
}

\seealso{
\code{\link{sdfFD}}
}

\examples{
sdfPLS(0.2, 100)
}

\keyword{ ts }
