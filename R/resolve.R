#' Resolve Guide
#'
#' Resolves implicit usage of default key 'guide' to explicit usage.
#' Generic, with method for data.frame.
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @return see methods
#' @family resolve
#' @examples
#' example(resolve.data.frame)
resolve <- function(x, ...)UseMethod('resolve')

#' Resolve Guide for Data Frame
#'
#' Resolves implicit usage of default key 'guide' to
#' explicit usage for data.frame.
#' Simply calls \code{\link{explicit_guide}}
#' followed by \code{\link{factorize_codelist}}.
#' @param x object
#' @param ... passed arguments
#' @export
#' @return data.frame
#' @family resolve
#' @family interface
#' @examples
#' library(magrittr)
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' x <- decorate(file)
#' x %>% resolve(default = 'unit') %>% as_yamlet
resolve.data.frame <- function(x, ...){
  x <- explicit_guide(x, ...)
  x <- factorize_codelist(x, ...)
  x
}
