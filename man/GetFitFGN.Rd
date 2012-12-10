\name{GetFitFGN}

\alias{GetFitFGN}

\title{ Fit FGN Time Series Model }

\description{
Exact maximum likelihood estimation of the parameter H
in fractional Gaussian noise (FGN).
This is a utility function used by \code{\link{FitFGN}} but
it is also useful in simulation experiments since it is 
faster than using \code{\link{FitFGN}}.
See example below.
In this model, alpha=2-2*H.
}

\usage{
GetFitFGN(z, MeanZeroQ = FALSE, algorithm=c("emle", "wmle"), ciQ=FALSE)
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
  \item{H}{MLE for H}
  \item{Loglikelihood}{value of the maximized loglikelihood}
  \item{alpha}{MLE for alpha}
  \item{algorithm}{either "emle" or "wmle"}
  \item{ci}{95 percent confidence interval for H}
}

\references{ 
McLeod, A.I., Yu, Hao, Krougly, Zinovi L.  (2007).
Algorithms for Linear Time Series Analysis,
Journal of Statistical Software.
}

\author{ A.I. McLeod}

\details{ The function \code{optimize} is used.
It is very rare but it has been observed that \code{optimize} can incorrectly choose
an endpoint. If this happens a warning is given and \code{optim} is used.
}

\seealso{ 
\code{\link{optimize}}, 
\code{\link{optim}}, 
\code{\link{Boot.FitFGN}}, 
\code{\link{FitFGN}},
\code{\link{FitRegressionFGN}} 
}

\examples{
#Example 1
#fit Gaussian White Noise, H=0.5
z<-rnorm(500, 100, 10)
GetFitFGN(z)

#Example 2
#estimate H for NileMin series
data(NileMin)
GetFitFGN(NileMin)

#Example 3
#Timing comparison for GetFitFGN and FitFGN
ns<-c(500,1000) #may extend this to other n's
H<-0.8
nR<-10
tim1<-tim2<-numeric(length(ns))
for (i in 1:length(ns)){
    n <- ns[i]    
    t1<-t2<-0
    s1<-proc.time()[1]
    for (iR in 1:nR){
        z<-SimulateFGN(n, H)
        H1<-GetFitFGN(z)
        }
    e1<-proc.time()[1]
    t1<-t1+(e1-s1)
    s2<-proc.time()[1]
    for (iR in 1:nR){
        z<-SimulateFGN(n, H)
        H2<-FitFGN(z)
        }
    e2<-proc.time()[1]
    t2<-t2+(e2-s2)
    tim1[i]<-t1
    tim2[i]<-t2
}
tb<-matrix(c(tim1,tim2),ncol=2)
dimnames(tb)<-list(ns,c("GetFitFGN","FitFGN"))

}

\keyword{ ts }
