\name{NileMin}
\alias{NileMin}
\docType{data}
\title{ Nile Annual Minima, 622 AD to 1284 AD}
\description{Annual Minimum flow of Nile River. See below for details.}
\usage{data(NileMin)}
\format{
  The format is:
 Time-Series [1:663] from 622 to 1284: 11.57 10.88 11.69 11.69  9.84 ...
 - attr(*, "title")= "Nile River minima series"
}
\details{
  The minimum annual level of the Nile has been recorded over many
  centuries and was given by Toussoun (1925). The data over the period
  622 AD to 1284 AD is considered more homogenous and reliable than
  the full dataset and has been analyzed by Beran (1994) and Percival and
  Walden (2000).  
  The full dataset is available  StatLib Datasets hipel-mcleod
  archive -- file: Minimum. 
}
\source{
Toussoun, O. (1925).
Memoire sur l'Histoire du Nil.
In Memoires a l'Institut d'Egypte, 18, 366-404.
}

\references{
Beran, J. (1994).
Statistics for Long-Memory Processes.
Chapman and Hall, New York.

Percival, D.B. and Walden, A.T. (2000)
Wavelet Methods for Time Series Analysis.
Cambridge University Press.
}
\examples{
#Example 1
#Compute Hurst's K estimate of H
data(NileMin)
HurstK(NileMin)
GetFitFGN(NileMin)

#Example 2
#Script for comparing FGN/ARMA forecast performance
#
\dontrun{
data(NileMin)
outNileMin<-FitFGN(NileMin)
set.seed(12177)
z<-Boot(outNileMin)
n<-length(z)
K<-100 #number of out-of-sample data values
z1<-z[1:(length(z)-K)] #training data
z2<-z[-(1:(length(z)-K))] #testing data
#
#FGN fit to z1 and forecast using z2
maxLead<-3
n1<-length(z1)
outz1<-FitFGN(z1)
H<-outz1$H
mu<-outz1$muHat
rFGN<-var(z1)*acvfFGN(H, n + maxLead -1)
F<-TrenchForecast(c(z1,z2), rFGN, mu, n1, maxLead=maxLead)$Forecasts
nF<-nrow(F)
err1<-z2-F[,1][-nF]
err2<-z2[-1]-F[,2][-c(nF,(nF-1))]
err3<-z2[-c(1,2)]-F[,3][-c(nF,(nF-1),(nF-2))]
rmse1<-sqrt(mean(err1^2))
rmse2<-sqrt(mean(err2^2))
rmse3<-sqrt(mean(err3^2))
FGNrmse<-c(rmse1,rmse2,rmse3)
#
#ARMA(p,q) fit to z1 and forecast using z2
p<-2
q<-1
ansz1<-arima(z1, c(p,0,q))
phi<-theta<-numeric(0)
if (p>0) phi<-coef(ansz1)[1:p]
if (q>0) theta<-coef(ansz1)[(p+1):(p+q)]
zm<-coef(ansz1)[p+q+1]
sigma2<-ansz1$sigma2
vz<-tacvfARMA(phi=phi, theta=theta, sigma2=sigma2, maxLag=0)
r<-vz*ARMAacf(ar=phi, ma=theta, lag.max=n + maxLead -1)
F<-TrenchForecast(c(z1,z2), r, zm, n1, maxLead=3)$Forecasts
err1<-z2-F[,1][-nF]
err2<-z2[-1]-F[,2][-c(nF,(nF-1))]
err3<-z2[-c(1,2)]-F[,3][-c(nF,(nF-1),(nF-2))]
rmse1<-sqrt(mean(err1^2))
rmse2<-sqrt(mean(err2^2))
rmse3<-sqrt(mean(err3^2))
ARMArmse<-c(rmse1,rmse2,rmse3)
#
#tabulate result
tb<-matrix(c(FGNrmse,ARMArmse),ncol=2)
dimnames(tb)<-list(c("lead1","lead2","lead3"),c("FGN","ARMA"))
}

}
\keyword{datasets}
