\name{earfima}
\alias{earfima}
\title{Exact MLE for ARFIMA}

\description{
The time series is corrected for the sample mean and then exact MLE is
used for the other parameters.
}

\usage{
earfima(z, order = c(0, 0, 0), lmodel = c("FD", "FGN", "PLA", "NONE"))
}

\arguments{
  \item{z}{time series}
  \item{order}{(p,d,q) where p=order AR, d=regular difference, q=order MA}
  \item{lmodel}{type of long-memory component: FD, FGN, PLA or NONE}
}

\details{The sample mean is asymptotically efficient.}

\value{
list with components:
	\item{bHat}{transformed optimal parameters} 
	\item{alphaHat}{estimate of alpha}
	\item{HHat}{estimate of H} 
	\item{dHat}{estimate of d} 
    \item{phiHat}{estimate of phi} 
	\item{thetaHat}{estimate of theta} 
	\item{wLL}{optimized value of Whittle approximate log-likelihood} 
    \item{LL}{corresponding exact log-likelihood}
	\item{convergence}{convergence indicator}
	\item{algorithm}{optimization algorithm used, 1 for L-BFGS-B, 2 for Nelder-Mead, 3 for SANN }
}

\author{Justin Veenstra and A. I. McLeod}


\examples{
z <- rnorm(100)
earfima(z, lmodel="FGN")
}

\keyword{ ts}
