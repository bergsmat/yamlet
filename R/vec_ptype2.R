# https://vctrs.r-lib.org/reference/howto-faq-coercion.html
#' @importFrom vctrs vec_ptype2
NULL

#' Find Common Type for dvec, dvec
#' 
#' Find common type for dvec, dvec.
#' @param x dvec
#' @param y dvec
#' @param ... passed arguments
#' @keywords internal
#' @export
#' @examples
#' vctrs::vec_ptype2(as_dvec('bar'), as_dvec(1))
#' vctrs::vec_ptype2(as_dvec(1), as_dvec('foo'))
#' vctrs::vec_ptype2(
#'   structure(as_dvec(1), guide = 'mg'),
#'   structure(as_dvec(1), guide = 'kg')
#' )
vec_ptype2.dvec.dvec <- function(x, y, ...) {
  # following type.convert():
  # logical > integer > numeric > (complex) > character
  z <- c(x, y)
  attributes(x) <- attributes(z)
  attributes(y) <- attributes(z)
  if(is.character(x)) return(x)
  if(is.character(y)) return(y)
  if(is.complex(x))   return(x)
  if(is.complex(y))   return(y)
  if(is.double(x))    return(x)
  if(is.double(y))    return(y)
  if(is.integer(x))   return(x)
  if(is.integer(y))   return(y)
  if(is.logical(x))   return(x)
  if(is.logical(y))   return(y)
  stop('unrecognized base type for dvec')
}

#' Find Common Type for dvec, logical
#' 
#' Find common type for dvec, logical.
#' @param x dvec
#' @param y logical
#' @param ... passed arguments
#' @keywords internal
#' @export
vec_ptype2.dvec.logical <- function(x, y, ...){
  x
}

#' Find Common Type for logical, dvec
#' 
#' Find common type for logical, dvec.
#' @param x logical
#' @param y dvec
#' @param ... passed arguments
#' @keywords internal
#' @export
vec_ptype2.logical.dvec <- function(x, y, ...){
  y
}

#' Find Common Type for dvec, integer
#' 
#' Find common type for dvec, integer.
#' @param x dvec
#' @param y integer
#' @param ... passed arguments
#' @keywords internal
#' @export
vec_ptype2.dvec.integer <- function(x, y, ...){
  x
}

#' Find Common Type for integer, dvec
#' 
#' Find common type for integer, dvec.
#' @param x integer
#' @param y dvec
#' @param ... passed arguments
#' @keywords internal
#' @export
vec_ptype2.integer.dvec <- function(x, y, ...){
  y
}

#' Find Common Type for dvec, double
#' 
#' Find common type for dvec, double.
#' @param x dvec
#' @param y double
#' @param ... passed arguments
#' @keywords internal
#' @export
vec_ptype2.dvec.double <- function(x, y, ...){
  x
}

#' Find Common Type for double, dvec
#' 
#' Find common type for double, dvec.
#' @param x double
#' @param y dvec
#' @param ... passed arguments
#' @keywords internal
#' @export
vec_ptype2.double.dvec <- function(x, y, ...){
  y
}

#' Find Common Type for dvec, dvec
#' 
#' Find common type for dvec, dvec.
#' @param x dvec
#' @param y dvec
#' @param ... passed arguments
#' @keywords internal
#' @export
vec_ptype2.dvec.character <- function(x, y, ...){
  x
}

#' Find Common Type for character, dvec
#' 
#' Find common type for character, dvec.
#' @param x character
#' @param y dvec
#' @param ... passed arguments
#' @keywords internal
#' @export
vec_ptype2.character.dvec <- function(x, y, ...){
  y
}

#' Find Common Type for dvec, complex
#' 
#' Find common type for dvec, complex.
#' @param x dvec
#' @param y complex
#' @param ... passed arguments
#' @keywords internal
#' @export
vec_ptype2.dvec.complex <- function(x, y, ...){
  x
}

#' Find Common Type for complex, dvec
#' 
#' Find common type for complex, dvec.
#' @param x complex
#' @param y dvec
#' @param ... passed arguments
#' @keywords internal
#' @export
vec_ptype2.complex.dvec <- function(x, y, ...){
  y
}
