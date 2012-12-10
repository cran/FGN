\name{sdfPLA}
\alias{sdfPLA}

\title{
Spectral density function for PLA
}
\description{
Computes the spectral density function for the PLA model with parameter a
at the Fourier frequencies, 2*Pi*j/n, j=1,...,[n/2],
where n is the length of the time series.
The evaluation is very fast. Bivariate interpolation and asymptotic approximation
are used.
}

\usage{
sdfPLA(a, n)
}

\arguments{
  \item{a}{
PLA parameter
}
  \item{n}{
length of time series
}
}
\details{
The details of the implementation are discussed in the accompanying vignette.
The parameter a should be in the interval (0,2).
series n should be greater than 2.
}
\value{
a vector of length [n/2] of the spectral density values.
}

\author{
A. I. McLeod and J. Veenstra
}

\seealso{
\code{\link{sdfFD}}
}

\examples{
sdfPLA(0.2, 100)
}

\keyword{ ts }
