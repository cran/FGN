\name{sdfhd}
\alias{sdfhd}
\title{Spectral density of hyperbolic decay models}

\description{
The spectral density function is evaluated at the Fourier frequencies, 
2*pi*seq(from=1/n, to=1/2, by=1/n) for various types of hyperbolic decay time series models.
}
\usage{
sdfhd(n, alpha = 1, phi = numeric(0), theta = numeric(0), lmodel = c("FD", "FGN", "PLA", "NONE"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{series length, number of Fourier frequencies [n/2]}
  \item{alpha}{canonical hyperbolic decay parameter}
  \item{phi}{autoregressive coefficients}
  \item{theta}{moving-average coefficients, Box-Jenkins style}
  \item{lmodel}{type of hyperbolic decay model}
}

\value{
vector of values of the spectral density function at the Fourier frequencies
}

\author{
A. I. McLeod
}


\examples{
sdfhd(100, 0.2)
}

\keyword{ ts }

