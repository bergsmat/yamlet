# https://vctrs.r-lib.org/reference/howto-faq-coercion.html
#' @importFrom vctrs vec_ptype2 vec_cast
NULL


#' @export
#' @examples
#' vctrs::vec_ptype2(as_dvec('bar'), as_dvec(1))
#' vctrs::vec_ptype2(as_dvec(1), as_dvec('foo'))
vec_ptype2.dvec.dvec <- function(x, y, ...) {
  z <- case_when(
    is.character(x) ~ x,
    is.character(y) ~ y,
    is.double(x) ~ x,
    is.double(y) ~ y,
    is.integer(x) ~ x,
    is.integer(y) ~ y,
    is.logical(x) ~ x,
    is.logical(y) ~ y
  )
  z
}

#' @export
vec_ptype2.dvec.logical <- function(x, y, ...){
  x
}

#' @export
vec_ptype2.logical.dvec <- function(x, y, ...){
  y
}

#' @export
vec_ptype2.dvec.integer <- function(x, y, ...){
  x
}

#' @export
vec_ptype2.integer.dvec <- function(x, y, ...){
  y
}

#' @export
vec_ptype2.dvec.double <- function(x, y, ...){
  x
}

#' @export
vec_ptype2.double.dvec <- function(x, y, ...){
  y
}

#' @export
vec_ptype2.dvec.character <- function(x, y, ...){
  x
}

#' @export
vec_ptype2.character.dvec <- function(x, y, ...){
  y
}
