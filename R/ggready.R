#' Prepare Data for GGplot
#'
#' Prepares data for ggplot.
#' Generic, with method for data.frame.
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @return see methods
#' @family resolve
#' @examples
#' example(ggready.data.frame)
ggready <- function(x, ...)UseMethod('ggready')

#' Prepare Data Frame for GGplot
#'
#' Prepares data.frame for ggplot. Calls
#' \code{\link{resolve}} and appends
#' units to label.
#' Enforces class 'decorated'.
#' If \code{parse} is true,
#' labels and units are
#' coerced with \code{\link{as_spork}}
#' and \code{\link{as_plotmath}},
#' merged with \code{\link[spork]{concatenate}},
#' and parsed with \code{\link{as.expression.plotmath}}.
#'
#'
#' @param x object
#' @param style passed to \code{\link{append_units}}
#' @param ... passed to \code{\link{append_units}}
#' @export
#' @importFrom spork as_spork
#' @importFrom spork plotmathToken
#' @return decorated
#' @family resolve
#' @family interface
#' @examples
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' x <- decorate(file)
#' x <- ggready(x)
#' str(x$conc)
#' library(magrittr)
#' library(ggplot2)
#' file %>%
#'  decorate %>%
#'  filter(!is.na(conc)) %>%
#'  ggready %>%
#'  ggplot(aes(x = time, y = conc, color = Heart)) +
#'  geom_point()


ggready.data.frame <- function(x, style = getOption('append_units_style','character'), ...){
  x <- resolve(x, ...)
  x <- append_units(x, style = style, ...)
  class(x) <- union('decorated', class(x))
  x
}

#' @export
spork::plotmathToken
