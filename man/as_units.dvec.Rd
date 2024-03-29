% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dvec.R
\name{as_units.dvec}
\alias{as_units.dvec}
\title{Coerce Decorated Vector to Units}
\usage{
\method{as_units}{dvec}(x, ..., preserve = getOption("yamlet_as_units_preserve", "label"))
}
\arguments{
\item{x}{dvec}

\item{...}{ignored}

\item{preserve}{attributes to preserve; just label by default (class and units are handled implicitly)}
}
\description{
Coerces dvec to units. If x has a units attribute,
it is used to create class 'units'. It is an error if 
x has no units attribute.
}
\examples{
library(magrittr)
a <- data.frame(id = 1:4, wt = c(70, 80, 70, 80), sex = c(0,1,0,1))
a \%<>\% decorate('wt: [ body weight, kg ]')
a \%<>\% decorate('sex: [ sex, [ female: 0, male: 1]]')
a \%<>\% decorate('id: identifier')
a \%<>\% resolve
a$wt \%>\% as_units
}
