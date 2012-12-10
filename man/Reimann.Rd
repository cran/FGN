\name{Reimann}
\alias{Reimann}
\title{
Computes Reimann zeta function on [0,3]
}
\description{
Computes Reimann zeta function on [0,3] by interpolating a table of values.
Since the function has a pole at 1, values of the function in the interval [0.999,1.001] are NA.
This function is used to define acfPLA.
}
\usage{
Reimann(v)
}
\arguments{
  \item{v}{
argument, between 0 and 3
}
}

\value{
The function value is returned.
}

\author{
A. I. McLeod
}

\seealso{
\code{\link{acvfPLA}}
}
\examples{
Reimann(0.5)
}

\keyword{math}
