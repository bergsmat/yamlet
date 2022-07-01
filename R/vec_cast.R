# https://vctrs.r-lib.org/reference/howto-faq-coercion.html
#' @importFrom vctrs vec_cast
NULL

#' Cast to dvec from dvec
#' 
#' Cast to dvec from dvec.
#' @param to dvec
#' @param x dvec
#' @param ... passed arguments
#' @keywords internal
#' @export
#' @importFrom vctrs vec_data
vec_cast.dvec.dvec <- function(x, to, ...) {
  # https://github.com/r-lib/rlang/issues/1432
  # as_dvec(x, ...) # during join, adds x_arg, to_arg, and 'call' attributes
  # logical, integer, double, complex, character
  # at <- attributes(x) # save these
  # y <- x
  # if(is.logical(to))   y <- as.logical(x)
  # if(is.integer(to))   y <- as.integer(x)
  # if(is.double(to))    y <- as.double(x)
  # if(is.complex(to))   y <- as.complex(x)
  # if(is.character(to)) y <- as.character(x)
  # attributes(y) <- at
  # y <- as_dvec(y)
  # y

  out <- vec_cast(vec_data(x), vec_data(to), ...)
  attributes(out) <- attributes(x)
  as_dvec(out)

}

#' Cast to dvec from logical
#' 
#' Cast to dvec from logical
#' @param to dvec
#' @param x logical
#' @param ... passed arguments
#' @keywords internal
#' @export
vec_cast.dvec.logical <- function(x, to, ...){
  # to dvec from logical
  # validate input?
  as_dvec(x)
}

#' Cast to logical from dvec
#' 
#' Cast to logical from dvec.
#' @param to logical
#' @param x dvec
#' @param ... passed arguments
#' @keywords internal
#' @export
vec_cast.logical.dvec <- function(x, to, ...){
  as.logical(x)
}

#' Cast to dvec from integer
#' 
#' Cast to dvec from integer.
#' @param to dvec
#' @param from integer
#' @param ... passed arguments
#' @keywords internal
#' @export
vec_cast.dvec.integer <- function(x, to, ...){
  as_dvec(x)
}

#' Cast to integer from dvec
#' 
#' Cast to integer from dvec.
#' @param to integer
#' @param from dvec
#' @param ... passed arguments
#' @keywords internal
#' @export
vec_cast.integer.dvec <- function(x, to, ...){
  as.integer(x)
}

#' Cast to dvec from double
#' 
#' Cast to dvec from double.
#' @param to dvec
#' @param from double
#' @param ... passed arguments
#' @keywords internal
#' @export
vec_cast.dvec.double <- function(x, to, ...){
  as_dvec(x)
}

#' Cast to double from dvec
#' 
#' Cast to double from dvec.
#' @param to double
#' @param from dvec
#' @param ... passed arguments
#' @keywords internal
#' @export
vec_cast.double.dvec <- function(x, to, ...){
  as.double(x)
}

#' Cast to dvec from character
#' 
#' Cast to dvec from character.
#' @param to dvec
#' @param from character
#' @param ... passed arguments
#' @keywords internal
#' @export
vec_cast.dvec.character <- function(x, to, ...){
  as_dvec(x)
}

#' Cast to character from dvec
#' 
#' Cast to character from dvec.
#' @param to character
#' @param from dvec
#' @param ... passed arguments
#' @keywords internal
#' @export
vec_cast.character.dvec <- function(x, to, ...){
  as.character(x)
}

#' Cast to dvec from complex
#' 
#' Cast to dvec from complex.
#' @param to dvec
#' @param from complex
#' @param ... passed arguments
#' @keywords internal
#' @export
vec_cast.dvec.complex <- function(x, to, ...){
  as_dvec(x)
}

#' Cast to complex from dvec
#' 
#' Cast to complex from dvec.
#' @param to complex
#' @param from dvec
#' @param ... passed arguments
#' @keywords internal
#' @export
vec_cast.complex.dvec <- function(x, to, ...){
  as.complex(x)
}
