\name{nuwaveletcoeff}
\alias{nuwaveletcoeff}
\title{ Computes one Wavelet Coefficient for Irregularly Sampled Time Series. }
\description{
    Computes one Wavelet Coefficient for Irregularly Sampled Time Series.
}
\usage{
nuwaveletcoeff(X, Y, t, o, wgt = cubicwgt, wgtrad = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{ \code{X} is the sequence of abscissa values. }
  \item{Y}{ \code{Y} is the sequence of ordinate values. }
  \item{t}{ \code{t} is the wavelet translation parameter for which the coefficient is to be computed }
  \item{o}{ \code{o} is the circular frequency for which the coefficient is to be computed. }
  \item{wgt}{ \code{wgt} is the used weight function, the default is cubicwgt. }
  \item{wgtrad}{ \code{wgtrad} determines the radius around 0 on which the weight function is greater than 0. }
}
\details{ }
\value{One complex spectral coefficient.}

\references{ http://basic-research.zkm.de }
\author{ Adolf Mathias <dolfi@zkm.de> }

\seealso{ \code{\link{nucorrcoeff}} }
\examples{
data(deut);nuwaveletcoeff(deut[[2]],deut[[4]],200000,1e-4);

## The function is currently defined as
function(X, Y, t, o, wgt=cubicwgt, wgtrad=1)
{   so <- 0.05*o
    rx <- subset(X, abs(X-t)*so<wgtrad)
    ry <- subset(Y, abs(X-t)*so<wgtrad)
    s <- sum(wgt((X-t)*so))
    if(s!=0)
        sum(wgt((rx-t)*so)*exp(1i*o*(rx-t))*ry)/s
    else
        0
  }
}
\keyword{ts}

