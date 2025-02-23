% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/singularity.R
\name{filter.decorated}
\alias{filter.decorated}
\title{Filter Decorated}
\usage{
\method{filter}{decorated}(
  .data,
  ...,
  .preserve = FALSE,
  .promote = getOption("yamlet_promote", TRUE)
)
}
\arguments{
\item{.data}{passed to \code{\link[dplyr]{filter}}}

\item{...}{passed to \code{\link[dplyr]{filter}}}

\item{.preserve}{passed to \code{\link[dplyr]{filter}}}

\item{.promote}{whether to auto-promote plural attributes}
}
\value{
decorated
}
\description{
Filters a decorated data.frame.
After a filter operation, promote()
is called to see if ambiguous conditional
attributes can be improved.
}
\examples{
library(magrittr)
file <- system.file(package = 'yamlet', 'extdata','phenobarb.csv')
x <- file \%>\% decorate

# Note that there are two elements each for value label and value guide.
x \%>\% decorations(event, value)

# Filtering promotes the relevant conditional attributes automatically.
x \%>\% filter(event == 'dose') \%>\% decorations(value)
x \%>\% filter(event == 'conc') \%>\% decorations(value)

}
\seealso{
Other promote: 
\code{\link{[.decorated}()},
\code{\link{promote}()},
\code{\link{promote.data.frame}()},
\code{\link{promote.list}()},
\code{\link{singularity}()}
}
\concept{promote}
\keyword{internal}
