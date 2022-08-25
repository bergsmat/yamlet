#' Resolve Guide
#'
#' Resolves implicit usage of default key 'guide' to explicit usage.
#' Generic, with method \code{\link{resolve.decorated}}.
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @return see methods
#' @family resolve
#' @examples
#' example(resolve.decorated)
resolve <- function(x, ...)UseMethod('resolve')

#' Resolve Guide for Decorated
#'
#' Resolves implicit usage of default key 'guide' to
#' explicit usage for decorated class.
#' Simply calls \code{\link{explicit_guide}}
#' followed by \code{\link{classified}}.
#' @param x decorated
#' @param ... passed to \code{\link{explicit_guide}} and \code{\link{classified}}
#' @export
#' @return decorated
#' @family resolve
#' @family interface
#' @examples
#' # generate some decorated data
#' library(magrittr)
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' x <- decorate(file)
#' x %>% decorations(Age, glyco)
#' 
#' # resolve everything, and show selected decorations
#' x %>% resolve %>% decorations(Age, glyco)
#' 
#' # resolve selectively, and show selected decorations
#' x %>% resolve(glyco) %>% decorations(Age, glyco)

resolve.decorated <- function(x, ...){
  x <- explicit_guide(x, ...)
  x <- classified(x, ...)
  # class(x) <- union('resolved', class(x))
  x
}
#' Resolve Guide for Decorated Vector
#'
#' Resolves implicit usage of default key 'guide' to
#' explicit usage for class dvec.
#' Simply calls \code{\link{explicit_guide}}
#' followed by \code{\link{classified}} if x has a codelist attribute.
#' @param x dvec
#' @param ... passed to \code{\link{explicit_guide}} and \code{\link{classified}}
#' @export
#' @keywords internal
#' @return dvec or classified
#' @family resolve
#' @family dvec
#' @examples
#' library(magrittr)
#' x <- as_dvec(1:3, guide = list(a = 1, b = 2, c = 3))
#' x %>% str
#' x %>% classified %>% str
#' x %>% explicit_guide %>% classified %>% str
#' x %>% resolve %>% str

resolve.dvec <- function(x, ...){
  x <- explicit_guide(x, ...)
  if('codelist' %in% names(attributes(x))){
    x <- classified(x, ...)
  }
  x
}
