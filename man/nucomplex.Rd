\name{nucomplex}
\alias{nucomplex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
nucomplex}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
nucomplex(X, Y, omegamax, ncoeff, noctave)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
%%     ~~Describe \code{X} here~~
}
  \item{Y}{
%%     ~~Describe \code{Y} here~~
}
  \item{omegamax}{
%%     ~~Describe \code{omegamax} here~~
}
  \item{ncoeff}{
%%     ~~Describe \code{ncoeff} here~~
}
  \item{noctave}{
%%     ~~Describe \code{noctave} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (X, Y, omegamax, ncoeff, noctave) 
.C("nucomplex", as.double(X), as.complex(Y), as.integer(min(length(X), 
    length(Y))), as.integer(ncoeff), as.integer(noctave), as.double(omegamax), 
    rp = complex(noctave * ncoeff), PACKAGE = "nuspectral")$rp
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
