\name{sdfFD}
\alias{sdfFD}

\title{
Spectral density function for FD
}
\description{
The spectral density function is evaluated at the Fourier frequencies, k/n, k=1,...,[n/2],
for FD (fractionally differenced white noise).
}
\usage{
sdfFD(d, n)
}

\arguments{
  \item{d}{
FD parameter
}
  \item{n}{
series length
}
}

\value{
vector of the sdf values
}

\author{
A. I. McLeod
}

\seealso{
\code{\link{GetFitFD}}
}

\examples{
sdfFD(0.2, 50)
}

\keyword{ ts }
