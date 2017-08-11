\name{wrap}
\alias{wrap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Apply a Function over a each combination of two lists or vectors. }
\description{A Helper Function for the Computation of Wavelet Coefficient Arrays.}
\usage{ wrap(x, y, my.fun, ...) }
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ \code{x} contains the array of first parameters given to \code{my.fun}. }
  \item{y}{ \code{y} contains the array of second parameters given to \code{my.fun}.  }
  \item{my.fun}{ \code{my.fun} is the function with at least two arguments to be applied to the table. }
  \item{\dots}{ Additional arguments for \code{my.fun} can be given in place of the \code{\dots}. }
}
\details{}
\value{ Returns an array of values of \code{my.fun} applied to all combination of values in \code{x} and \code{y}.}

 ~Make other sections like Warning with \section{Warning }{....} ~ 
\examples{
data(co2);outer(seq(-420000, 0, 10000), 2*pi/(120000*2^seq(-4,0,0.0833333)), wrap,
                function(t,o) nuwaveletcoeff(co2[[2]], co2[[4]], -t, o));

## The function is currently defined as
function(x, y, my.fun, ...) 
    sapply(seq(along=x), function(i) my.fun(x[[i]], y[[i]], ...))
}
\keyword{ts}
