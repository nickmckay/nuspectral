\name{nurealcoeff}
\alias{nurealcoeff}
\title{Computes a Spectral Coefficient for Irregularly Sampled Real Time Series. }
\description{ The function \code{nurealcoeff} computes one spectral coefficient
for irregularly sampled real data.
}
\usage{
nurealcoeff(X, Y, o)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{ \code{X} is the sequence of abscissa values. }
  \item{Y}{ \code{Y} is the sequence of ordinate values. }
  \item{o}{ \code{o} is the circular frequency for which the coefficient is to be computed. }
}
\details{}
\value{One spectral coefficient in complex representation.}
\references{ http://basic-research.zkm.de }
\author{ Adolf Mathias <dolfi@zkm.de> }
\note{One complex spectral correlation coefficient.}

\seealso{ \code{\link{nureal}},\code{\link{fastnureal}} }
\examples{data(deut);

## The function is currently defined as
function(X,Y,o)
{   if(o!=1)
    {   n     <- length(X)
        zeta  <- sum(Y*exp(-1i*o*X))
        iota2 <- sum(exp(-2i*o*X))
        2*(n*Conj(zeta)-Conj(iota2)*zeta)/(n*n-Conj(iota2)*iota2)
    }
    else
    {   mean(Y)   }
  }
}
\keyword{ts}

