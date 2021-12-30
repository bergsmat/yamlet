#' Merge Decorated
#'
#' Preserves class for 'decorated' during merge().
#'
#'
#' @param x decorated
#' @param y passed to \code{\link{merge}}
#' @param ... passed to \code{\link{merge}}
#' @return class 'decorated' 'data.frame'
#' @export
#' @family decorated
#' @examples
#' library(magrittr)
#' library(dplyr)
#' x <- data.frame(foo = 1, bar = 2)
#' x %<>% decorate('foo: [distance, mm]')
#' x %<>% decorate('bar: [height, mm]')
#' class(merge(x,x))
merge.decorated <- function(x, y, ...){
  z <- NextMethod()
  z <- as_decorated(z)
  z
}
