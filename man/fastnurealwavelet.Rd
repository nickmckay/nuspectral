\name{fastnurealwavelet}
\alias{fastnurealwavelet}
\title{Fast Algorithm for Spectral Estimation of Irregularly Sampled Data over a Logarithmic
Frequency Range.}
\description{ The function \code{fastnurealwavelet} computes a spectrum of irregularly sampled data. The
resulting coefficients are represented as complex numbers, which stems
from the fact that the accelerated final summation is of complex
nature. The computation uses a divide-and-conquer scheme and allows
dramatic speedups compared to \code{\link{nureal}}.
}
\usage{
fastnurealwavelet(X, Y, omegamax, ncoeff, noctave, tmin, tmax, tsubdiv, sigma=0.1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{ \code{X} is the ordered sequence of abscissa values. }
  \item{Y}{ \code{Y} is the sequence of corresponding ordinate values. }
  \item{omegamax}{ \code{omegamax} is the top circular frequency for which the spectrum is to be computed. }
  \item{ncoeff}  { \code{ncoeff} is the number of coefficients evenly distributed per octave to be calculated. }
  \item{noctave} { \code{noctave} is the number of octaves to be calculated. } 
  \item{tmin}    { \code{tmin} is the minimum translation value for which wavelet coefficients are to be calculated. } 
  \item{tmax}    { \code{tmax} is the maximum translation value for which wavelet coefficients are to be calculated. } 
  \item{tsubdiv} { \code{tsubdiv} specifies the number of translation values for which wavelet coefficients are to be calculated. } 
  \item{sigma}   { \code{sigma} specifies the length of the wavelet support, i.e. the time/frequency tradeoff; the default value of 0.1
                   means that 10 periods of exp(i t) fit into the wavelet window, smaller values increase the window size }
  }
\value{An array of spectral coefficients in complex representation.}
\references{ http://basic-research.zkm.de }
\author{ Adolf Mathias <dolfi@zkm.de> }
\note{}

\seealso{\code{\link{nureal}}, \code{\link{fastnureal}}, \code{\link{nuwaveletcoeff}}}

\examples{data(deut); fastnurealwavelet(deut[[2]],deut[[4]],1e-4,16,4, 0, 420000, 200);

## The function is currently defined as
function(X, Y, omegamax, ncoeff, noctave, tmin, tmax, tsubdiv, sigma=0.1)
  .C("fastnureal",
    as.double(X),
    as.double(Y),
    as.integer(min(length(X),length(Y))),
    as.double(X[[length(X)]]-X[[1]]),
    as.integer(ncoeff),
    as.integer(noctave),
    as.double(tmin),
    as.double(tmax),
    as.integer(tsubdiv),
    as.double(sigma),
    as.double(omegamax),
    rp = complex(noctave*ncoeff*tsubdiv))$rp
    }
\keyword{ts}
