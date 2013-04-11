\name{warfima}
\alias{warfima}

\title{Whittle mle arfima}

\description{
Fit time series arfima model using Whittle approximate maximum likelihood estimation.
}

\usage{
warfima(z, order = c(0, 0, 0), lmodel = c("FD", "FGN", "PLA", "PLS", "NONE"))
}

\arguments{
  \item{z}{vector of time series values}
  \item{order}{c(p,d,q), where is AR order, d is differencing, q is MA order}
  \item{lmodel}{type of hyperbolic decay model}
}


\value{
list with components:
	\item{bHat}{transformed optimal parameters} 
	\item{alphaHat}{estimate of alpha}
	\item{HHat}{estimate of H} 
	\item{dHat}{estimate of d} 
        \item{phiHat}{estimate of phi} 
	\item{thetaHat}{estimate of theta} 
	\item{LL}{optimized value of Whittle approximate log-likelihood} 
	\item{convergence}{convergence indicator}
	\item{algorithm}{optimization algorithm used, 1 for L-BFGS-B, 2 for Nelder-Mead, 3 for SANN }
}

\author{
A. I. McLeod
}


\examples{
warfima(NileMin, lmodel="FGN")

}

\keyword{ ts}

