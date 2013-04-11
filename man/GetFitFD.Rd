\name{GetFitFD}
\alias{GetFitFD}
\title{
Fit FD Time Series Model
}
\description{
Exact maximum likelihood estimation of the parameter d
in fractionally-differenced white noise (FD).
In this model, alpha=1-2*d.
}
\usage{
GetFitFD(z, MeanZeroQ = FALSE, algorithm=c("emle","wmle"), ciQ=FALSE)
}

\arguments{
  \item{z}{ time series data vector }
  \item{MeanZeroQ}{ optional argument, default is MeanZeroQ=FALSE. 
  Set to TRUE if the mean is known to be zero }
  \item{algorithm}{"emle" or "wmle" for exact or Whittle mle}
  \item{ciQ}{TRUE or FALSE according as 95 percent confidence interval computed and plotted}
}

\value{
a list with four/five elements:
  \item{d}{MLE for d}
  \item{Loglikelihood}{value of the maximized loglikelihood}
  \item{alpha}{MLE for alpha}
  \item{algorithm}{either "emle" or "wmle"}
  \item{ci}{95 percent confidence interval for d}
}



\author{
A. I. McLeod
}


\seealso{
\code{\link{GetFitFGN}}
}

\examples{
#Example 1
#fit Gaussian White Noise, d=0
z<-rnorm(500, 100, 10)
GetFitFD(z)

#Example 2
#estimate d for NileMin series
data(NileMin)
GetFitFD(NileMin)
GetFitFD(NileMin, algorithm="wmle")

}

\keyword{ ts }
