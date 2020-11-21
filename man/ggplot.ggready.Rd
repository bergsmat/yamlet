% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggplot_ggready.R
\name{ggplot.ggready}
\alias{ggplot.ggready}
\title{Create a New ggplot for a GGready Data Frame}
\usage{
\method{ggplot}{ggready}(data, ...)
}
\arguments{
\item{data}{data.frame or similar}

\item{...}{passed to \code{\link[ggplot2]{ggplot}}}
}
\value{
return value like \code{\link[ggplot2]{ggplot}}
}
\description{
Creates a new ggplot object for a ggready data.frame.
This is the ggplot() method for class 'ggready';
it tries to implement automatic labels and units in axes and legends
in association with \code{\link{print.dg}}.
This approach is deprecated in favor of a cleaner
object hierarchy for classes 'decorated' and 'resolved'.
}
\examples{
meta <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
x <- decorate(meta)
library(ggplot2)
class(ggplot(data = x) + geom_path(aes(x = time, y = conc)))
class(ggplot(data = x, aes(x = time, y = conc)) + geom_path())
example(print.dg)
}
\seealso{
Other dg: 
\code{\link{ggplot_build.dg}()},
\code{\link{print.dg}()}
}
\concept{dg}
\keyword{internal}