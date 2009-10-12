\name{SimulateFGN}
\alias{SimulateFGN}

\title{ Simulates FGN }
\description{
A fractional Gaussian noise time series is simulated.
}
\usage{
SimulateFGN(n, H)
}

\arguments{
  \item{n}{ length of time series}
  \item{H}{ Hurst coefficient }
}
\details{
  The FFT is used so it is most efficient if you select n to be a power of 2.
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
\code{\link[ltsa]{DLSimulate}} 
}
\examples{
#Example 1
#simulate a process with H=0.2 and plot it
z<-SimulateFGN(100, 0.2)
ts.plot(z)
# 
#Example 2
#simulate FGN and compare theoretical and sample autocovariances
H<-0.7
n<-8192
z<-SimulateFGN(n, H)
#autocovariances
sacvf<-acf(z, plot=FALSE,type="covariance")$acf
tacf<-FGNAcf(0:(n-1), H)
tb<-matrix(c(tacf[1:10],sacvf[1:10]),ncol=2)
dimnames(tb)<-list(0:9, c("Tacvf","Sacvf"))
tb
}
\keyword{ ts }
\keyword{ datagen }

