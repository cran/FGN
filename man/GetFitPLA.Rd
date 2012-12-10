\name{GetFitPLA}
\alias{GetFitPLA}

\title{
Fit PLA Time Series Model
}
\description{
Exact maximum likelihood estimation of the decay parameter a in PLA model.
In this model a=alpha.
}
\usage{
GetFitPLA(z, MeanZeroQ = FALSE, algorithm=c("emle", "wmle"), ciQ=FALSE)
}
\arguments{
  \item{z}{ time series data vector }
  \item{MeanZeroQ}{ optional argument, default is MeanZeroQ=FALSE. 
  Set to TRUE if the mean is known to be zero }
  \item{algorithm}{"emle" for exact MLE or "wmle" for Whittle method}
  \item{ciQ}{TRUE or FALSE according as 95 percent confidence interval computed and plotted}
}

\value{
a list with four/five elements:
  \item{p}{MLE for a}
  \item{Loglikelihood}{value of the maximized loglikelihood}
  \item{alpha}{MLE for a}
  \item{ci}{95 percent confidence interval for a}
  \item{algorithm}{either "emle" or "wmle"}
}

\author{
A. I. McLeod and Justin Veenstra
}


\seealso{
\code{\link{GetFitFD}}
}

\examples{
#Example 1
#fit Gaussian White Noise, alpha=1
z<-rnorm(500, 100, 10)
GetFitPLA(z)

#Example 2
#estimate alpha for NileMin series
data(NileMin)
GetFitPLA(NileMin)
}

\keyword{ ts }


