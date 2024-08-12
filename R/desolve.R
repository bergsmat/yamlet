#' Desolve Guide
#'
#' Un-resolves explicit versions of 'guide' to implicit usage.
#' Generic, with methods 
#' \code{\link{desolve.decorated}},
#' \code{\link{desolve.classified}}, and 
#' \code{\link{desolve.dvec}}.
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
#' implicit usage for 'decorated' class.
#' Simply calls 
#' \code{\link{drop_title}}, 
#' \code{\link{unclassified}},
#' and \code{\link{implicit_guide}}. 
#' @param x decorated
#' @param ... passed to \code{\link{drop_title}}, \code{\link{unclassified}}, and \code{\link{implicit_guide}}
#' @export
#' @return decorated
#' @family resolve
#' @family interface
#' @examples
#' library(magrittr)
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' x <- decorate(file)
#' 
#' # this is how Age, glyco, Race look when resolved
#' x %>% resolve %>% decorations(Age, glyco, Race)
#' 
#' # we can resolve two of them and then 'unresolve' all of them
#' x %>% resolve(glyco, Race) %>% desolve %>% decorations(Age, glyco, Race)

desolve.decorated <- function(x, ...){
  x <- drop_title(x, ...)
  x <- unclassified(x, ...)
  x <- implicit_guide(x, ...)
  x
}
#' Desolve Guide for Classified
#'
#' Un-resolves explicit usage of default key 'guide' to
#' implicit usage for class 'classified'.
#' Calls 
#' \code{\link{drop_title}} (a non-action by default),
#' \code{\link{unclassified}},
#' followed by \code{\link{implicit_guide}}.
#' @param x classified
#' @param ... passed to \code{\link{drop_title}}, \code{\link{unclassified}}, and \code{\link{unclassified}}
#' @export
#' @return dvec
#' @family resolve
#' @family classified
#' @examples
#' library(magrittr)
#' x <- as_dvec(
#'   4:6, 
#'   guide = list(a = 4L, b = 5L, c = 6L)
#' )
#' 
#' # untouched
#' x %>% str
#' 
#' # resolved
#' x %>% resolve %>% str
#' 
#' # resolved and desolved
#' x %>% resolve %>% desolve %>% str

desolve.classified <- function(x, ...){
  x <- drop_title(x, ...)
  x <- unclassified(x, ...)
  x <- implicit_guide(x, ...)
  x
}
#' Desolve Guide for Decorated Vector
#'
#' Un-resolves explicit usage of default key 'guide' to
#' implicit usage for class dvec.
#' Calls 
#' \code{\link{drop_title}},
#' \code{\link{unclassified}},
#' and \code{\link{implicit_guide}}.
#' @param x 
#' @param ... passed to \code{\link{drop_title}}, \code{\link{unclassified}}, and \code{\link{implicit_guide}}
#' @export
#' @return dvec
#' @family resolve
#' @family dvec
#' @keywords internal
#' @examples
#' library(magrittr)
#' x <- as_dvec(4:6)
#' attr(x, 'guide') <- 'kg'
#' x %>% str
#' x %>% resolve %>% str
#' x %>% resolve %>% desolve %>% str

desolve.dvec <- function(x, ...){
  x <- drop_title(x, ...)
  x <- unclassified(x, ...)
  x <- implicit_guide(x, ...)
  x
}

#' Desolve Data Frame
#' 
#' Desolves data.frame.
#' Coerces first using as_decorated().
#' 
#' @param x data.frame
#' @param ... ignored
#' @export
#' @keywords internal
#' @return decorated
#' @family resolve
#' @examples
#' head(desolve(Theoph))
desolve.data.frame <- function(x, ...){
  #desolve(as_decorated(x, ...), ...)
  # @ 1.0.3: above, first use of dots 
  # can pass anonymous args to decorate.list,
  # which may understand one of them
  # as 'meta' and issue an error
  desolve(as_decorated(x), ...)
}
