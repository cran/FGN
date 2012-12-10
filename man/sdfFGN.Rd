\name{sdfFGN}
\alias{sdfFGN}

\title{
Spectral density function for FGN
}
\description{
Computes the spectral density function for the FGN model with parameter H
at the Fourier frequencies, 2*Pi*j/n, j=1,...,[n/2],
where n is the length of the time series.
The evaluation is very fast since bivariate interpolation is used.
}

\usage{
sdfFGN(H, n)
}

\arguments{
  \item{H}{
FGN parameter
}
  \item{n}{
length of time series
}
}
\details{
The details of the implementation are discussed in the accompanying vignette.
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
sdfFGN(0.7, 100)
}

\keyword{ ts }
