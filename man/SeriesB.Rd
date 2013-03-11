\name{SeriesB}
\alias{SeriesB}
\docType{data}
\title{Series B, close price IBM stock
}
\description{
Closing price of IBM common stock, daily, May 17 1961 to November 2 1962
}
\usage{data(SeriesB)}
\format{
  The format is:
 num [1:369] 460 457 452 459 462 459 463 479 493 490 ...
}

\source{
G. E. P. Box, G.E.P., Jenkins, G.M. and Reinsel, G.C. (2008). Time Series Analysis: Forecasting and Control, 4th Ed., Wiley.
}



\examples{
r <- diff(log(SeriesB))
a <- abs(diff(log(SeriesB)))
layout(c(2,1))
acf(r, main="log returns")
acf(a, main="absolute log returns")
#
}
\keyword{datasets}
