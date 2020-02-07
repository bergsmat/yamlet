#' Coerce to Unit String
#'
#' Coerces to class 'unit_string'. Generic,
#' with method \code{\link{as_unit_string.character}}.
#' A unit string is character text suitable
#' as input for \code{\link[units]{as_units}}.
#' See also \code{\link{is_parseable}}.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family unit_string
#' @return unit_string
#' @md
#' @examples
#' as_unit_string('kg.m^2/s^2')
#' as_unit_string('kg.m2 s-2')

as_unit_string <- function(x, ...)UseMethod('as_unit_string')

#' Coerce Character to Unit String
#'
#' Coerces character to class 'unit_string'.
#' See description for \code{\link{as_unit_string}}.
#'
#' @param x character
#' @param ... ignored arguments
#' @export
#' @keywords internal
#' @family unit_string
#' @return unit_string
#' @examples
#' as_unit_string('kg m2 s-2')
as_unit_string.character <- function(x, ...){
  class(x) <- union('unit_string', class(x))
  x
}

#' Coerce Factor to Unit String
#'
#' Coerces factor to class 'unit_string'
#' by converting to character and calling
#' \code{\link{as_unit_string}}.
#'
#' @param x factor
#' @param ... ignored arguments
#' @export
#' @keywords internal
#' @family unit_string
#' @return unit_string
#' @examples
#' as_unit_string(as.factor('kg m2 s-2'))
as_unit_string.factor <- function(x, ...)as_unit_string(as.character(x), ...)

#' Subset Unit String
#'
#' Subsets unit_string, retaining class.
#' @param x unit_string
#' @param ... passed to next method
#' @export
#' @keywords internal
#' @family unit_string
#' @return unit_string
#' @examples
#' x <- c(
#'   'm',
#'   's',
#'   'ng/mL'
#' )
#' x <- as_unit_string(x)
#' class(x)
#' class(x[1])
`[.unit_string` <- function(x, ...){
  y <- NextMethod()
  # contrasts and levels will have been handled
  class(y) <- union('unit_string', class(y))
  y
}
#' Element-select Unit String
#'
#' Element-selects unit_string, retaining class.
#' @param x unit_string
#' @param ... passed to next method
#' @export
#' @keywords internal
#' @family unit_string
#' @return unit_string
#' @examples
#' x <- c(
#'   'm',
#'   's',
#'   'ng/mL'
#' )
#' x <- as_unit_string(x)
#' class(x)
#' class(x[[1]])
`[[.unit_string` <- function(x, ...){
  y <- NextMethod()
  # contrasts and levels will have been handled
  class(y) <- union('unit_string', class(y))
  y
}

#' Coerce Symbolic Units to Unit String.
#'
#' Coerces symbolic units to unit_string.
#' @param x symbolic_units; see \code{\link[units]{as_units}}
#' @param canonical whether to return the form with all positive exponents
#' @param ... ignored arguments
#' @export
#' @keywords internal
#' @importFrom units as_units
#' @importFrom units deparse_unit
#' @family unit_string
#' @return unit_string
#' @examples
#' library(units)
#' x <- as_units('kg.m/s^2')
#' names(attributes(x))
#' y <- attr(x,'units')
#' class(y)
#' as.character(y)
#' as.character(attr(x, 'units'))
#' as_unit_string(y)
#' library(magrittr)
#' 'kg.m^2/s^2' %>% as_units %>% attr('units') %>% as_unit_string
#' 'kg.m2 s-2' %>% as_units %>% attr('units') %>% as_unit_string
#' 'kg.m^2/s^2' %>% as_units %>% attr('units') %>% as_unit_string(FALSE)
#' 'kg.m2 s-2' %>% as_units %>% attr('units') %>% as_unit_string(FALSE)
as_unit_string.symbolic_units <- function(x, canonical = TRUE, ...){
  y <- as.character(x)
  if(!canonical)y <- as_units(y) %>% deparse_unit
  y <- as_unit_string(y, ...)
  y
}

#' Coerce Units to Unit String.
#'
#' Coerces units to unit_string. Extracts units
#' attribute (of class(symbolic_units)) and converts.
#' @param x units; see \code{\link[units]{as_units}}
#' @param ... ignored arguments
#' @export
#' @keywords internal
#' @importFrom units as_units
#' @importFrom units deparse_unit
#' @family unit_string
#' @return unit_string
#' @examples
#' library(units)
#' x <- as_units('kg.m/s^2')
#' as_unit_string(x)
#' as_unit_string(x, canonical = FALSE)
#'
as_unit_string.units <- function(x, canonical = TRUE, ...){
  y <- attr(x, 'units')
  y <- as_unit_string(y, canonical = canonical, ...)
  y
}

