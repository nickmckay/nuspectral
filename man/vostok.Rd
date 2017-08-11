\name{vostok}
\alias{co2}
\alias{deut}
\alias{o18}
\alias{ch4}
\alias{dust}
\docType{data}
\title{ Vostok Data }
\description{
  Paleoclimatic data extracted from the Vostok ice core that was
  extracted from a 3 km deep hole from the ice shield of Lake Vostok, Antarctica.
}
\usage{data(co2)
data(deut)
data(o18)
data(ch4)
data(dust)
}
\format{
  \code{co2} is a data frame with 283 observations on the following 4 variables.
  \describe{
    \item{Depth..corrected.}{Corrected ice depth in meters.}
    \item{Ice.age..GT4.}{Ice age in years.}
    \item{Gas.age}{}
    \item{CO2..ppmv.}{Carbon dioxide content in air bubbles enclosed in
    the ice.}
  }
  \code{deut} is a data frame with 3311 observations on the following 4 variables.
  \describe{
    \item{Depth..corrected.}{Corrected ice depth in meters.}
    \item{Ice.age..GT4.}{Ice age in years.}
    \item{deut}{Deuterium content.}
    \item{deltaTS}{Temperature derived from Deuterim content in degree C.}
  }
  \code{o18} is A data frame with 318 observations on the following 4 variables.
  \describe{
    \item{Depth..corrected.}{Corrected ice depth in meters.}
    \item{Ice.age..GT4.}{Ice age in years.}
    \item{gas.age..GT4.}{Gas age in years.}
    \item{Atm.O18}{18-Oxygen content relative to total Oxygen content in in air bubbles enclosed in
    the ice.}
  }
  \code{ch4} is a data frame with 458 observations on the following 4 variables.
  \describe{
    \item{Depth..corrected.}{Corrected ice depth in meters.}
    \item{Ice.age..GT4.}{Ice age in years.}
    \item{Gas.age}{Gas age in years.}
    \item{CH4..ppbv.}{Methane concentration.}
  }
  \code{dust} is a data frame with 522 observations on the following 3 variables.
  \describe{
    \item{depth.corrected}{Corrected ice depth in meters.}
    \item{Ice.age..GT4.}{Ice age in years.}
    \item{Dust.Conc..ppm.}{Dust concentration in the ice.}
  }
}

\source{Petit, J.R. et al.: Climate and atmospheric history of the
past 420 000 years from the Vostok ice core, Antarctica. 399,
 Nature (399): pp. 429--436, 1999.
}
\references{
 http://www.nature.com/cgi-taf/DynaPage.taf?file=/nature/journal/v399/n6735/full/399429a0\_fs.html.
}
\keyword{datasets}
