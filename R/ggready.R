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
#' \code{\link{resolve}} and one of
#' \code{\link{join_label_units}} or
#' \code{\link{express_label_units}}.
#' Enforces class 'decorated'.
#'
#' @param x object
#' @param parse if TRUE, call \code{\link{express_label_units}} instead of \code{\link{join_label_units}}
#' @param ... passed arguments
#' @export
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


ggready.data.frame <- function(x, parse = TRUE, ...){
  x <- explicit_guide(x, ...)
  x <- factorize_codelist(x, ...)
  if(parse) x <- express_label_units(x,...)
  if(!parse) x <- join_label_units(x,...)
  class(x) <- union('decorated', class(x))
  x
}
