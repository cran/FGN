\name{FGNLL}
\alias{FGNLL}
\title{ Concentrated Loglikelihood Function for H }
\description{
The concentrated loglikelihood, that is, the loglikelihood function
maximized over the innovation variance parameter, is computed.
}
\usage{
FGNLL(H, z)
}
\arguments{
  \item{H}{ parameter }
  \item{z}{ data vector, assumed to be mean corrected }
}
\value{
the value of the loglikelihood
}

\author{ A.I. McLeod}

\references{ 
McLeod, A.I., Yu, Hao, Krougly, Zinovi L.  (2007).
Algorithms for Linear Time Series Analysis,
Journal of Statistical Software.
}


\seealso{ 
\code{\link{FitFGN}},
\code{\link[ltsa]{DLLoglikelihood}} 
}

\examples{
#compute loglikelihood for NileFlowCMS with H=0.9
data(NileFlowCMS)
z<-NileFlowCMS
z<-z-mean(z)
FGNLL(0.9, z)


#simulate Gaussian white noise and tabulate the loglikelihood for H=0.40, 0.45, 0.50, 0.55, 0.60
set.seed(4321)
h<-c(0.40, 0.45, 0.50, 0.55, 0.60)
z<-rnorm(500, 100, 50)
z<-z-mean(z)
LL<-numeric(length(h))
for (i in 1:length(h))
	LL[i]<-FGNLL(h[i],z)
matrix(c(h,LL),ncol=2)
}
\keyword{ ts }
