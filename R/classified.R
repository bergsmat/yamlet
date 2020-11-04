#' Coerce to Classified
#'
#' Coerce something to classified.
#' Generic, with method for factor.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family classified
#' @return see methods
#' @examples
#' example(as_classified.factor)
as_classified <- function(x, ...)UseMethod('as_classified')

#' Coerce Factor to Classified
#'
#' Coerce factor to classified.
#' Creates a factor that retains attributes during subsetting.
#'
#' @param x factor
#' @param ... ignored arguments
#' @export
#' @keywords internal
#' @family classified
#' @return class 'classified' 'factor'
#' @examples
#' class(as_classified(factor(letters)))
as_classified.factor <- function(x, ...){
  class(x) <- union('classified', class(x))
  x
}

#' Subset Classified.
#'
#' Subsets classified factor, retaining attributes.
#' @param x classified factor
#' @param ... passed to next method
#' @export
#' @keywords internal
#' @family classified
#' @return class 'classified' 'factor'
#' @examples
#' a <- as_classified(factor(letters))
#' attr(a, 'label') <- 'foo'
#' a <- a[1:3]
#' attributes(a)

`[.classified` <- function(x, ...){
  y <- NextMethod()
  # contrasts and levels will have been handled
  nms <- names(attributes(x))
  nms <- setdiff(nms, c('contrasts','levels'))
  for(nm in nms){
    attr(y, nm) <- attr(x, nm)
  }
  y
}

