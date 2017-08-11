\name{nucorrcoeff}
\alias{nucorrcoeff}
\title{Computes a Windowed Spectral Correlation Coefficient}
\description{ The function \code{nurealcoeff} computes one spectral coefficient
for two sets of irregularly sampled complex or real data.}
\usage{
nucorrcoeff(X1, Y1, X2, Y2, t, o, wgt = cubicwgt, wgtrad = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X1}{ \code{X1} is the sequence of abscissa values for the first time series. }
  \item{Y1}{ \code{Y1} is the sequence of ordinate values for the first time series. }
  \item{X2}{ \code{X2} is the sequence of abscissa values for the second time series. }
  \item{Y2}{ \code{Y2} is the sequence of ordinate values for the second time series. }
  \item{t}{ \code{t} is the wavelet translation parameter for which the coefficient is to be computed }
  \item{o}{ \code{o} is the circular frequency for which the coefficient is to be computed. }
  \item{wgt}{ \code{wgt} is the used weight function, the default is cubicwgt. }
  \item{wgtrad}{ \code{wgtrad} determines the radius around 0 on which the weight function is greater than 0. }
}
\details{}
\value{One complex spectral correlation coefficient.}
\references{ http://basic-research.zkm.de }
\author{ Adolf Mathias <dolfi@zkm.de> }
\seealso{ \code{\link{nuwaveletcoeff}} }
\examples{
data(deut);data(co2);nucorrcoeff(co2[[2]],co2[[4]],deut[[2]],deut[[4]],200000,1e-4);

## The function is currently defined as
function(X1, Y1, X2, Y2, t, o, wgt=cubicwgt, wgtrad=1)
{   so <- 0.05*o
    rx1 <- subset(X1, abs(X1-t)*so<wgtrad)
    ry1 <- subset(Y1, abs(X1-t)*so<wgtrad)
    rx2 <- subset(X2, abs(X2-t)*so<wgtrad)
    ry2 <- subset(Y2, abs(X2-t)*so<wgtrad)
    s = sum(wgt((rx1-t)*so)) * sum(wgt((rx2-t)*so))
    if(s!=0)
        sum(wgt((rx1-t)*so)*exp(1i*o*(rx1))*ry1) *
        sum(wgt((rx2-t)*so)*exp(1i*o*(rx2))*ry2) / s
    else
        0
}
}
\keyword{ts}
