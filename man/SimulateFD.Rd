\name{SimulateFD}
\alias{SimulateFD}

\title{ Simulates FD }
\description{
A fractional Gaussian noise time series is simulated.
}
\usage{
SimulateFD(n, d)
}

\arguments{
  \item{n}{ length of time series}
  \item{d}{ fractional difference parameter }
}
\details{
  The FFT is used so it is most efficient if you select n to be a power of 2. Note, d=H-1/2.
}
\value{
vector of length containing the simulated time series
}

\author{ A.I. McLeod}

\references{ 
Davies,  R. B. and Harte, D. S. (1987).
Tests for Hurst Effect. Biometrika 74,  95--101.

McLeod, A.I., Yu, Hao, Krougly, Zinovi L.  (2007).
Algorithms for Linear Time Series Analysis,
Journal of Statistical Software.
}

\seealso{ 
\code{\link{SimulateFGN}},
\code{\link[ltsa]{DLSimulate}} 
}
\examples{
#Example 1
#simulate a process with H=0.2 and plot it
z<-SimulateFD(100, 0.2)
ts.plot(z)
}
\keyword{ ts }
\keyword{ datagen }

