\name{sdfarma}
\alias{sdfarma}

\title{ARMA spectral density function}

\description{
The spectral density function is evaluated at the Fourier frequencies, 
2*pi*seq(from=1/n, to=1/2, by=1/n) for the ARMA model.
}

\usage{sdfarma(n, phi = numeric(0), theta = numeric(0))}

\arguments{
  \item{n}{length of series, number of Fourier frequencies is [n/2]}
  \item{phi}{autoregressive coefficients}
  \item{theta}{moving-average coefficients}
}

\value{vector of spectral density values corresponding to the Fourier frequencies}

\author{A. I. McLeod}


\examples{
sdfarma(100, 0.9, 0.7)
}


\keyword{ ts }

