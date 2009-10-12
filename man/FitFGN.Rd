\name{FitFGN}
\alias{FitFGN}
\title{ MLE estimation for FGN }
\description{
Exact MLE estimation for FGN
}
\usage{
FitFGN(z, demean = TRUE, MeanMLEQ = FALSE, lag.max = "default")
}

\arguments{
  \item{z}{ time series, vector or ts object. }
  \item{demean}{ if True, subtract mean. Otherwise assume it is zero. }
  \item{MeanMLEQ}{ if True, an iterative algorithm is used for exact simultaneous
  MLE estimation of the mean and other parameters.}
  \item{lag.max}{ the residual autocorrelations are tabulated for lags 1, \dots, lag.max. Also
  lag.max is used for the Ljung-Box portmanteau test. }
}

\details{
The exact loglikelihood function is maximized numerically using \code{optimize}.
The standard error for the H parameter is estimated (McLeod, Yu and Krougly, 2007).
}

\value{
  A list with class name "FitAR" and components: 
  \item{loglikelihood }{value of the loglikelihood}
  \item{H}{estimate of H parameter}
  \item{SEH}{SE of H estimate}
  \item{sigsqHat }{innovation variance estimate}
  \item{muHat }{estimate of the mean}
  \item{SEmu }{SE of mean}
  \item{Rsq }{R-squared, coefficient of forecastability}
  \item{LjungBox}{table of Ljung-Box portmanteau test statistics}
  \item{res}{normalized residuals, same length as z}
  \item{demean }{TRUE if mean estimated otherwise assumed zero}
  \item{IterationCount }{number of iterations in mean mle estimation}
  \item{MLEMeanQ }{TRUE if mle for mean algorithm used}
  \item{tsp}{tsp(z)}
  \item{call}{result from match.call() showing how the function was called}
  \item{DataTitle}{returns attr(z,"title") }
}

\references{
McLeod, A.I., Yu, Hao, Krougly, Zinovi L.  (2007).
Algorithms for Linear Time Series Analysis,
Journal of Statistical Software.
}

\author{ A.I. McLeod }

\seealso{ 
\code{\link{GetFitFGN}}, 
\code{\link{FitRegressionFGN}}, 
\code{\link{Boot.FitFGN}},
\code{\link{coef.FitFGN}},
\code{\link{plot.FitFGN}},
\code{\link{print.FitFGN}},
\code{\link{summary.FitFGN}},
\code{\link{HurstK}}
}

\examples{
data(NileMin)
out<-FitFGN(NileMin)
summary(out)
plot(out)
coef(out)
}

\keyword{ ts }
