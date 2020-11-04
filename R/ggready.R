#' Prepare Data for GGplot
#'
#' Prepares data for ggplot.
#' Generic, with method for data.frame.
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @return see methods
#' @family deprecated
#' @examples
#' example(ggready.data.frame)
ggready <- function(x, ...)UseMethod('ggready')

#' Prepare Data Frame for GGplot
#'
#' Prepares data.frame for ggplot. Calls
#' \code{\link{resolve}} and appends
#' units to label using \code{\link{append_units}}
#' (passing \code{style = 'plotmath'} if \code{parse}
#' is true, else \code{style = 'plain'}).
#' Enforces class 'ggready'.
#'
#'
#' @param x object
#' @param ... passed to \code{\link{append_units}} and \code{\link{resolve}}; may include unquoted column names
#' @param parse passed to \code{\link{append_units}}
#' @export
#' @importFrom spork as_spork
#' @importFrom spork plotmathToken
#' @return ggready
#' @family deprecated
#' @keywords internal
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
#' file %>% decorate %>% ggready(time, Heart:glyco) %>% as_yamlet

ggready.data.frame <- function(
  x, ... ,
  parse = getOption('ggready_parse',TRUE)
){
  x <- resolve(x, ...)
  stopifnot(is.logical(parse), length(parse) == 1)
  x <- append_units(x, ..., style = if(parse) 'plotmath' else 'plain')
  class(x) <- union('decorated', class(x))
  class(x) <- union('ggready', class(x))
  x
}

#' @export
spork::plotmathToken
