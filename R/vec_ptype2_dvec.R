# https://vctrs.r-lib.org/reference/howto-faq-coercion.html
#' @importFrom vctrs vec_ptype2
NULL

#' Find Common Type for dvec, dvec
#' 
#' Find common type for dvec, dvec.
#' @param x dvec
#' @param y dvec
#' @param ... ignored
#' @keywords internal
#' @export
#' @examples
#' str(vctrs::vec_ptype2(as_dvec(1L), as_dvec(1)))
#' str(vctrs::vec_ptype2(as_dvec(1), as_dvec(1L)))
#' str(vctrs::vec_ptype2(as_dvec(TRUE), as_dvec(1L)))
#' str(vctrs::vec_ptype2(as_dvec(TRUE), as_dvec(1)))
#' str(vctrs::vec_ptype2(as_dvec(1), as_dvec(1+0i)))
#' 
#' vctrs::vec_ptype2(
#'   structure(as_dvec(1), guide = 'mg'),
#'   structure(as_dvec(1), guide = 'kg')
#' )
vec_ptype2.dvec.dvec <- function(x, y, ...) {
  z <- c(x, y)
  z <- unclass(z) # restored below
  p <- vec_ptype2(unclass(x), unclass(y))
  attributes(p) <- attributes(z)
  p <- as_dvec(p)
  p
}

#' Find Common Type for dvec, logical
#' 
#' Find common type for dvec, logical.
#' @param x dvec
#' @param y logical
#' @param ... ignored
#' @keywords internal
#' @export
vec_ptype2.dvec.logical <- function(x, y, ...){
  z <- c(x, y)
  z <- unclass(z) # restored below
  p <- vec_ptype2(unclass(x), y)
  attributes(p) <- attributes(z)
  p <- as_dvec(p)
  p
}

#' Find Common Type for logical, dvec
#' 
#' Find common type for logical, dvec.
#' @param x logical
#' @param y dvec
#' @param ... ignored
#' @keywords internal
#' @export
vec_ptype2.logical.dvec <- function(x, y, ...){
  z <- c(as_dvec(x), y)
  z <- unclass(z) # restored below
  p <- vec_ptype2(x, unclass(y))
  attributes(p) <- attributes(z)
  p <- as_dvec(p)
  p
}

#' Find Common Type for dvec, integer
#' 
#' Find common type for dvec, integer.
#' @param x dvec
#' @param y integer
#' @param ... ignored
#' @keywords internal
#' @export
vec_ptype2.dvec.integer <- function(x, y, ...){
  z <- c(x, y)
  z <- unclass(z) # restored below
  p <- vec_ptype2(unclass(x), y)
  attributes(p) <- attributes(z)
  p <- as_dvec(p)
  p
}

#' Find Common Type for integer, dvec
#' 
#' Find common type for integer, dvec.
#' @param x integer
#' @param y dvec
#' @param ... ignored
#' @keywords internal
#' @export
vec_ptype2.integer.dvec <- function(x, y, ...){
  z <- c(as_dvec(x), y)
  z <- unclass(z) # restored below
  p <- vec_ptype2(x, unclass(y))
  attributes(p) <- attributes(z)
  p <- as_dvec(p)
  p
}

#' Find Common Type for dvec, double
#' 
#' Find common type for dvec, double.
#' @param x dvec
#' @param y double
#' @param ... ignored
#' @keywords internal
#' @export
#' @examples
#' str(vctrs::vec_ptype2(as_dvec(1), 1))
#' str(vctrs::vec_ptype2(1, as_dvec(1)))
#' 
#' str(vctrs::vec_ptype2(as_dvec(1, label = 'x'), 1))
#' str(vctrs::vec_ptype2(1, as_dvec(1, label= 'x')))
#' str(vctrs::vec_ptype2(as_dvec(1), structure(1, label = 'x')))
#' 

vec_ptype2.dvec.double <- function(x, y, ...){
  z <- c(x, y)
  z <- unclass(z) # restored below
  p <- vec_ptype2(unclass(x), y)
  attributes(p) <- attributes(z)
  p <- as_dvec(p)
  p
}

#' Find Common Type for double, dvec
#' 
#' Find common type for double, dvec.
#' @param x double
#' @param y dvec
#' @param ... ignored
#' @keywords internal
#' @export
vec_ptype2.double.dvec <- function(x, y, ...){
  z <- c(as_dvec(x), y)
  z <- unclass(z) # restored below
  p <- vec_ptype2(x, unclass(y))
  attributes(p) <- attributes(z)
  p <- as_dvec(p)
  p
}

#' Find Common Type for dvec, character
#' 
#' Find common type for dvec, character
#' @param x dvec
#' @param y dvec
#' @param ... ignored
#' @keywords internal
#' @export
vec_ptype2.dvec.character <- function(x, y, ...){
  z <- c(x, y)
  z <- unclass(z) # restored below
  p <- vec_ptype2(unclass(x), y)
  attributes(p) <- attributes(z)
  p <- as_dvec(p)
  p
}

#' Find Common Type for character, dvec
#' 
#' Find common type for character, dvec.
#' @param x character
#' @param y dvec
#' @param ... ignored
#' @keywords internal
#' @export
vec_ptype2.character.dvec <- function(x, y, ...){
  z <- c(as_dvec(x), y)
  z <- unclass(z) # restored below
  p <- vec_ptype2(x, unclass(y))
  attributes(p) <- attributes(z)
  p <- as_dvec(p)
  p
}

#' Find Common Type for dvec, complex
#' 
#' Find common type for dvec, complex.
#' @param x dvec
#' @param y complex
#' @param ... ignored
#' @keywords internal
#' @export
vec_ptype2.dvec.complex <- function(x, y, ...){
  z <- c(x, y)
  z <- unclass(z) # restored below
  p <- vec_ptype2(unclass(x), y)
  attributes(p) <- attributes(z)
  p <- as_dvec(p)
  p
}

#' Find Common Type for complex, dvec
#' 
#' Find common type for complex, dvec.
#' @param x complex
#' @param y dvec
#' @param ... ignored
#' @keywords internal
#' @export
vec_ptype2.complex.dvec <- function(x, y, ...){
  z <- c(as_dvec(x), y)
  p <- vec_ptype2(x, unclass(y))
  attributes(p) <- attributes(z)
  p <- as_dvec(p)
  p
}
