#' Check Parseable as Units
#'
#' Checks if something is parseable as units.
#' Generic, with default method.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @return see methods
#' @family parseable
#' @examples
#' example(is_parseable.character)
is_parseable <- function(x,...)UseMethod('is_parseable')


#' Check Something is Parseable as Units by Default
#'
#' Checks if something is parseable as units.
#' Tests against the udunits library in \pkg{units}.
#' See \code{\link[units]{as_units}}.
#' See also \code{\link[units]{install_symbolic_unit}}
#' for finer control.
#'
#' @param x character
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @importFrom units as_units
#' @return logical
#' @family parseable
#' @family interface
#' @examples
#' is_parseable(c('kg/m2','kg/m^2','kg.m/s2','µg/L'))
#' is_parseable('foo')
#' library(units)
#' install_symbolic_unit('foo')
#' is_parseable('foo')
#'
is_parseable.default <- function(x,...){
  res <- sapply(x, .is_parseable.default, ..., USE.NAMES = FALSE)
  res
}

.is_parseable.default <- function(x,...){
  stopifnot(length(x) == 1)
  res <- try(
    silent = TRUE,
    suppressWarnings(
      suppressMessages(
        as_units(x,...)
      )
    )
  )
  res <- !inherits(res, 'try-error')
  res
}


