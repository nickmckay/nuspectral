\name{nureal}
\alias{nureal}
\title{Algorithm for Spectral Estimation of Irregularly Sampled Data over a Logarithmic
Frequency Range.}
\description{ The function \code{nureal} computes a spectrum of irregularly sampled data. The
resulting coefficients are represented as complex numbers, which stems
from the fact that the accelerated final summation is of complex
nature.
}
\usage{
nureal(X, Y, omegamax, ncoeff, noctave)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{ \code{X} is the ordered sequence of abscissa values. }
  \item{Y}{ \code{Y} is the sequence of corresponding ordinate values. }
  \item{omegamax}{ \code{omegamax} is the top circular frequency for which the spectrum is to be computed. }
  \item{ncoeff}{ \code{ncoeff} is the number of coefficients evenly distributed per octave to be calculated. }
  \item{noctave}{ \code{noctave} is the number of octaves to be calculated. } }
}
\value{An array of spectral coefficients in complex representation.}
\references{ http://basic-research.zkm.de }
\author{ Adolf Mathias <dolfi@zkm.de> }
\note{}

\seealso{\code{\link{nureal}}}

\examples{data(deut); nureal(deut[[2]],deut[[4]],1e-4,16,4);}


## The function is currently defined as
function(X, Y, xlength, omegamax, ncoeff, noctave)
 .C("nureal",
    as.double(X), as.double(Y), as.integer(min(length(X),length(Y))),
    rp = complex(noctave*ncoeff), as.integer(ncoeff), as.integer(noctave))$rp
}
\keyword{ts}
