% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/isometric.R
\name{isometric}
\alias{isometric}
\title{Enforce Isometry}
\usage{
isometric()
}
\value{
ggplot_isometric
}
\description{
Enforces isometric plot design:  aspect ratio of 1, identical 
ranges for x and y axes. Can be used meaningfully with
\code{+ facet_wrap(scales = 'free' ...)}.
}
\examples{
library(magrittr)
library(ggplot2)
data.frame(x = 1:5, y = 3:7) \%>\%
ggplot(aes(x, y)) + geom_point() + isometric()
}
\seealso{
ggplot_add.ggplot_isometric

Other isometric: 
\code{\link{ggplot_add.ggplot_isometric}()},
\code{\link{ggplot_add.ggplot_symmetric}()},
\code{\link{symmetric}()}
}
\concept{isometric}
\keyword{internal}
