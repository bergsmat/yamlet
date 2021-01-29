#' Desolve Guide
#'
#' Un-resolves explicit versions of 'guide' to implicit usage.
#' Generic, with method \code{\link{desolve.decorated}}.
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @return see methods
#' @family resolve
#' @examples
#' example(resolve.decorated)
desolve <- function(x, ...)UseMethod('desolve')

#' Desolve Guide for Decorated
#'
#' Un-resolves explicit usage of default key 'guide' to
#' implicit usage for decorated class.
#' Simply calls \code{\link{unclassified}}
#' followed by \code{\link{implicit_guide}}.
#' @param x object
#' @param ... passed to \code{\link{implicit_guide}} and \code{\link{classified}}
#' @export
#' @return decorated
#' @family resolve
#' @family interface
#' @examples
#' library(magrittr)
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' x <- decorate(file)
#' x %>% resolve %>% decorations(Age, glyco, Race)
#' x %>% resolve(glyco, Race) %>% desolve %>% decorations(Age, glyco, Race)

desolve.decorated <- function(x, ...){
  x <- unclassified(x, ...)
  x <- implicit_guide(x, ...)
  #class(x) <- setdiff(class(x),'resolved')
  x
}
