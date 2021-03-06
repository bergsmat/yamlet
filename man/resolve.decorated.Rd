% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resolve.R
\name{resolve.decorated}
\alias{resolve.decorated}
\title{Resolve Guide for Decorated}
\usage{
\method{resolve}{decorated}(x, ...)
}
\arguments{
\item{x}{object}

\item{...}{passed to \code{\link{explicit_guide}} and \code{\link{classified}}}
}
\value{
decorated
}
\description{
Resolves implicit usage of default key 'guide' to
explicit usage for decorated class.
Simply calls \code{\link{explicit_guide}}
followed by \code{\link{classified}}.
}
\examples{
library(magrittr)
file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
x <- decorate(file)
x \%>\% resolve \%>\% decorations(Age, glyco)
x \%>\% resolve(glyco) \%>\% decorations(Age, glyco)
}
\seealso{
Other resolve: 
\code{\link{desolve.decorated}()},
\code{\link{desolve}()},
\code{\link{resolve}()}

Other interface: 
\code{\link{classified.data.frame}()},
\code{\link{decorate.character}()},
\code{\link{decorate.data.frame}()},
\code{\link{desolve.decorated}()},
\code{\link{ggplot.decorated}()},
\code{\link{io_csv.character}()},
\code{\link{io_csv.data.frame}()},
\code{\link{io_res.character}()},
\code{\link{io_table.character}()},
\code{\link{io_table.data.frame}()},
\code{\link{io_yamlet.character}()},
\code{\link{io_yamlet.data.frame}()},
\code{\link{is_parseable.default}()},
\code{\link{mimic.default}()},
\code{\link{modify.default}()},
\code{\link{promote.list}()},
\code{\link{read_yamlet}()},
\code{\link{selected.default}()},
\code{\link{write_yamlet}()}
}
\concept{interface}
\concept{resolve}
