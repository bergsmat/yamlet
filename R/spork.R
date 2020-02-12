#' Coerce Symbolic Units to Spork
#'
#' Coerces symbolic units to spork by coercing first
#' to unit_string.
#' @param x symbolic_units; see \code{\link[units]{as_units}}
#' @param ... ignored arguments
#' @export
#' @keywords internal
#' @family util
#' @return spork
#' @examples
#' library(units)
#' x <- as_units('kg.m/s^2')
#' names(attributes(x))
#' y <- attr(x,'units')
#' class(y)
#' as.character(y)
#' as.character(attr(x, 'units'))
#' as_spork(y)
#' library(magrittr)
#' 'kg.m^2/s^2' %>% as_units %>% attr('units') %>% as_spork
#' 'kg.m2 s-2' %>% as_units %>% attr('units') %>% as_spork
#' 'kg.m^2/s^2' %>% as_units %>% attr('units') %>% as_spork(FALSE)
#' 'kg.m2 s-2' %>% as_units %>% attr('units') %>% as_spork(FALSE)

as_spork.symbolic_units <- function(x, canonical = TRUE, ...){
  y <- as_unit_string(x, canonical = canonical, ...)
  y <- as_spork(y, ...)
  y
}

#' Coerce Units to Spork
#'
#' Coerces units to spork by coercing first
#' to unit_string.
#' @param x units; see \code{\link[units]{as_units}}
#' @param ... ignored arguments
#' @export
#' @keywords internal
#' @family spork
#' @return spork
#' @examples
#' library(units)
#' library(magrittr)
#' 'kg.m^2/s^2' %>% as_units %>% as_spork
#' 'kg.m2 s-2' %>% as_units %>% as_spork
#' 'kg.m^2/s^2' %>% as_units %>% as_spork(FALSE)
#' 'kg.m2 s-2' %>% as_units %>% as_spork(FALSE)
as_spork.units <- function(x, canonical = TRUE, ...){
  y <- as_unit_string(x, canonical = canonical, ...)
  y <- as_spork(y, ...)
  y
}

#' Coerce Unit String to Spork
#'
#' Coerces unit string to spork.  A literal dot
#' means different things in spork vs. units,
#' and there may be some other subtleties as well.
#' Unit string is character that \code{\link{is_parseable}}.
#' @param x unit_string
#' @param ... ignored arguments
#' @export
#' @keywords internal
#' @family spork
#' @return units
#' @examples
#' library(magrittr)
#' 'kg.m^2/s^2' %>% as_unit_string %>% as_spork
#' 'kg.m2 s-2' %>% as_unit_string %>% as_spork
as_spork.unit_string <- function(x, ...){
  stopifnot(all(is_parseable(x)))
  y <- gsub('\\.','*',x) # \u22c5 https://en.wikipedia.org/wiki/Interpunct
  y <- gsub('\\^([0-9])+','^\\1.',y) # canonical, all pos num follow ^
  y <- gsub('([a-zA-Z])([-0-9]+)', '\\1^\\2.',y) # non-canonical, unsigned or neg num follow char
  y <- as_spork(as.character(y))
  y
}
