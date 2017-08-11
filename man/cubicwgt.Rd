\name{cubicwgt}
\alias{cubicwgt}

\title{Algorithms for Spectral Analysis of Irregularly Sampled Time Series}
\description{
  The function \code{cubicwgt} is a replacement for the popular Hanning window
  with comparable spectral properties that is used as standard weight
  function in nuwavelet and that is faster to compute since it is a
  polynomial of degree 3.
}
\usage{
cubicwgt(p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{ Abscissa value, the window is greater than 0 for -1 < \code{p} < 1.  }
}
\value{  Weight function value between 0 and 1. }
\references{ http://basic-research.zkm.de }
\author{ Adolf Mathias <dolfi@zkm.de> }
\note{ }
\seealso{ \code{\link{nuwaveletcoeff}}, \code{\link{nucorrcoeff}} }
\examples{cubicwgt(0.5)

## The function is currently defined as
function(t) { a<-abs(t); ifelse(a<1,1+a*a*(2*a-3),0) }
}
\keyword{ts}
