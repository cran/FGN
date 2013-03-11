\name{WLoglikelihood}
\alias{WLoglikelihood}

\title{Whittle log-likelihood}

\description{The Whittle and exact log-likelihoods are evaluated}

\usage{
WLoglikelihood(z, beta, p, q, lmodel)
}

\arguments{
  \item{z}{time series}
  \item{beta}{vector of parameters - see details}
  \item{p}{AR order}
  \item{q}{MA order}
  \item{lmodel}{Type of long-memory component: FD, FGN, PLA, PLS, NONE}
}

\details{
If the model has a long-memory component then beta[1] is alpha, where alpha is the canonical parameter.
For FD models, d=0.5 - alpha/2. For FGN, H=1-alpha/2.
If there is no long-memory component, beta[1] starts with the first ARMA parameter, where the 
ARMA parameters correspond to the autoregressive coefficients followed by the moving-average
coefficients.
}

\value{
vector of length two with entries corresponding to the value of the log-likelihood
for Whittle and exact methods.
}

\author{
A. I. McLeod
}


\examples{
z <- globtp-mean(globtp)
WLoglikelihood(z, c(0.2, 0.9, 0.2), p=1, q=1, lmodel="FGN")
}

\keyword{ ts }

