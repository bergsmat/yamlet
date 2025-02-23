% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unit_string.R
\name{as_unit_string.symbolic_units}
\alias{as_unit_string.symbolic_units}
\title{Coerce Symbolic Units to Unit String.}
\usage{
\method{as_unit_string}{symbolic_units}(x, canonical = TRUE, ...)
}
\arguments{
\item{x}{symbolic_units; see \code{\link[units]{as_units}}}

\item{canonical}{whether to return the form having all positive exponents}

\item{...}{ignored arguments}
}
\value{
unit_string
}
\description{
Coerces symbolic units to unit_string.
}
\examples{
library(units)
x <- as_units('kg.m/s^2')
names(attributes(x))
y <- attr(x,'units')
class(y)
as.character(y)
as.character(attr(x, 'units'))
as_unit_string(y)
library(magrittr)
'kg.m^2/s^2' \%>\% as_units \%>\% attr('units') \%>\% as_unit_string
'kg.m2 s-2' \%>\% as_units \%>\% attr('units') \%>\% as_unit_string
'kg.m^2/s^2' \%>\% as_units \%>\% attr('units') \%>\% as_unit_string(FALSE)
'kg.m2 s-2' \%>\% as_units \%>\% attr('units') \%>\% as_unit_string(FALSE)
}
\seealso{
Other unit_string: 
\code{\link{[.unit_string}()},
\code{\link{[[.unit_string}()},
\code{\link{as_unit_string}()},
\code{\link{as_unit_string.character}()},
\code{\link{as_unit_string.factor}()},
\code{\link{as_unit_string.units}()}
}
\concept{unit_string}
\keyword{internal}
