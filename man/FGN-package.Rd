\name{FGN-package}
\alias{FGN-package}
\docType{package}

\title{
Fractional Gaussian Noise and hyperbolic decay time series model fitting
}

\description{
Exact and Whittle MLE for time series models with hyperbolic decay.
Simulation and regression supported for FGN.
}

\details{
	\tabular{ll}{
	Package: \tab FGN\cr
	Type: \tab Package\cr
	Version: \tab 2.0-5\cr
	Date: \tab 2013-04-02\cr
	License: \tab CC BY-NC-SA 3.0\cr
	LazyLoad: \tab yes\cr
	LazyData: \tab yes\cr
	}
}


\author{
A. I. McLeod and Justin Veenstra

Maintainer: aimcleod@uwo.ca
}

\references{ 
Hipel, K.W. and McLeod, A.I., (2005).
Time Series Modelling of Water Resources and Environmental Systems.
Electronic reprint of our book orginally published in 1994.
\url{http://www.stats.uwo.ca/faculty/aim/1994Book/}.

McLeod, A.I., Yu, Hao, Krougly, Zinovi L.  (2007).
Algorithms for Linear Time Series Analysis,
Journal of Statistical Software.

McLeod, A.I. and Veenstra, Justin (2012).
Hyperbolic Decay Time Series Models (in press).
}

\keyword{ts}

\keyword{package}

\seealso{
\code{\link{HurstK}}, 
\code{\link{FitFGN}}, 
\code{\link{FitRegressionFGN}},
\code{\link{SimulateFGN}},
\code{\link{print.FitFGN}}, 
\code{\link{summary.FitFGN}},
\code{\link{predict.FitFGN}},
\code{\link{plot.FitFGN}},
\code{\link{residuals.FitFGN}},
\code{\link{GetFitFGN}},
\code{\link{GetFitFD}},
\code{\link{GetFitPLS}},
\code{\link{GetFitPLA}}
}		

\examples{
#Example 1
#Compare HurstK and MLE for H
#Hurst K for Nile Minima
data(NileMin)
HurstK(NileMin)
out<-FitFGN(NileMin)
summary(out)
plot(out)
coef(out)
#
#Example 2.
#Compare models
\dontrun{
   T1 <- proc.time()[3]
   ansFD <- GetFitFD(NileMin)
   T2 <- proc.time()[3]
   ansFGN <- GetFitFGN(NileMin)
   T3 <- proc.time()[3]
   ansPLS <- GetFitPLS(NileMin)
   T4 <- proc.time()[3]
   ansPLA <- GetFitPLA(NileMin)
   T5 <- proc.time()[3]
   tbLLE <- c(ansFD[[2]],ansFGN[[2]],ansPLS[[2]],ansPLA[[2]])
   est <-  c(ansFD[[3]],ansFGN[[3]],ansPLS[[3]],ansPLA[[3]])
   tbLL <- round(tbLLE, 2)
   est <- round(est, 3)
   T<-c(T2-T1,T3-T2,T4-T3,T5-T4)
   m<-matrix(c(est,tbLL, T),nrow=4, ncol=3)
   dimnames(m)<-list(list("FD","FGN","PLS","PLA"), list("alpha","logL", "time"))
   mE <- m
   mE
   #
   T1 <- proc.time()[3]
   ansFD <- GetFitFD(NileMin, algorithm="wmle")
   T2 <- proc.time()[3]
   ansFGN <- GetFitFGN(NileMin, algorithm="wmle")
   T3 <- proc.time()[3]
   ansPLA <- GetFitPLS(NileMin, algorithm="wmle")
   T4 <- proc.time()[3]
   ansPLS <- GetFitPLA(NileMin, algorithm="wmle")
   T5 <- proc.time()[3]
   #tbLL <- c(ansFD[[2]],ansFGN[[2]],ansPLS[[2]],ansPLA[[2]])
   z <- NileMin-mean(NileMin)
   tbLLW <- c(LLFD(ansFD[[1]],z), LLFGN(ansFGN[[1]],z), LLPLS(ansPLS[[1]],z), LLPLA(ansPLA[[1]],z))
   est <-  c(ansFD[[3]],ansFGN[[3]],ansPLS[[3]],ansPLA[[3]])
   tbLL <- round(tbLLW, 2)
   est <- round(est, 3)
   T<-c(T2-T1,T3-T2,T4-T3,T5-T4)
   m<-matrix(c(est,tbLL, T),nrow=4, ncol=3)
   dimnames(m)<-list(list("FD","FGN","PLS","PLA"), list("alpha","logL", "time"))
   mW<-m
   mW
   m<-cbind(mE,mW)
   m
   }
  }
