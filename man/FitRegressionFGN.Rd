\name{FitRegressionFGN}
\alias{FitRegressionFGN}
\title{ Regression with FGN Errors  }
\description{
   Fits a multiple linear regression with FGN errors
}
\usage{
FitRegressionFGN(X, y)
}
\arguments{
  \item{X}{ design matrix, must include column of 1's if constant term is present }
  \item{y}{ the response variable, a time series }
}
\details{
	An iterative algorithm is used to compute the exact MLE.
}
\value{
a list with 3 elements:
  \item{Loglikelihood}{value of the maximized loglikelihood}
  \item{H}{MLE for H}
  \item{alpha}{MLE for regression coefficients corresponding to colums of X}
}

\references{ 
McLeod, A.I., Yu, Hao, Krougly, Zinovi L.  (2007).
Algorithms for Linear Time Series Analysis,
Journal of Statistical Software.
}

\author{ A.I. McLeod}

\note{ It is assumed that X is not collinear. }

\seealso{ 
\code{\link{FitFGN}}, 
\code{\link{lsfit}} 
}

\examples{
#simulate FGN with mean zero and H=0.2 and fit exact mle for H and mean
H<-0.2
z<-SimulateFGN(512, H)
mean(z)
X<-matrix(rep(1,length(z)), ncol=1)
ans<-FitRegressionFGN(X,z)
ans


#fit a step intervention model to the Nile annual riverflow data
data(NileFlowCMS)
n<-length(NileFlowCMS)
X<-matrix(c(rep(1,n),rep(0,32),rep(1,n-32)),ncol=2)
ans<-FitRegressionFGN(X,NileFlowCMS)
ans
}

\keyword{ ts }
\keyword{ regression }
\keyword{ nonlinear }
