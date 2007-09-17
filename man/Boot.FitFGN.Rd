\name{Boot.FitFGN}
\alias{Boot.FitFGN}
\title{ Simulate Fitted FGN Model }
\description{
Simulate a realization from a fitted AR model. This is useful in the
parametric bootstrap.  Generic function for "Boot" method.
}
\usage{
Boot.FitFGN(obj, R = 1, ...)
}
\arguments{
  \item{obj}{ the output from FitAR }
  \item{R}{ number of bootstrap replications}
  \item{...}{ optional arguments }
}
\details{
The method of Davies and Harte (1987) is used if it is applicable, otherwise
the Durbin-Levinsion recursion is used.
}

\value{
If R=1, a simulated time series with the same length as the original fitted time series 
is produced. Otherwise if R>1, a matrix with R columns and number of rows
equal to the length of the series containing R replications of the bootstrap.
}

\references{ 
McLeod, A.I., Yu, Hao, Krougly, Zinovi L.  (2007).
Algorithms for Linear Time Series Analysis,
Journal of Statistical Software.
 }

\author{ A.I. McLeod }
\seealso{
\code{\link{SimulateFGN}}, 
\code{\link[LTSA]{DHSimulate}} 
\code{\link[LTSA]{DLSimulate}} 
 }

\examples{
#Example 1
#Fit a FGN model and determine the bootstrap sd of H
#Measure cpu time. With R=250, it takes about 23 sec 
#on 3.6 GHz Pentium IV.
data(NileMin)
outNileMin<-FitFGN(NileMin)
start<-proc.time()[1]
R<-25
Hs<-numeric(R)
Z<-Boot(outNileMin, R=R)
for (i in 1:R)
    Hs[i]<-GetFitFGN(Z[,i])$H
BootSD<-sd(Hs) #this is the bootstrap sd
end<-proc.time()[1]
totTim<-end-start
}

\keyword{ ts }
