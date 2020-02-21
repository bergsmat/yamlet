#' Render Attribute
#'
#' Renders Attribute
#' Generic, with methods \code{\link{render_attr.default}}
#' and \code{\link{render_attr.data.frame}}.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @return see methods
#' @examples
#' # see methods
render_attr <- function(x, ...)UseMethod('render_attr')


#' Render Attribute by Default
#'
#' Renders Attribute by default.
#'
#'
#' @param x object
#' @param which attribute name (exact match); see \code{\link{attr}}
#' @param fun function or function name to process attribute
#' @param ... passed arguments
#' @export
#' @return same class as x with updated attribute
#' @examples
#' library(magrittr)
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' x <- file %>% decorate
#' attributes(x$time)
#' render_attr(x$time, 'label', toupper) %>% attributes

render_attr.default <- function(x, which, fun = concatenate, ...){
  fun <- match.fun(fun)
  att <- attr(x, which, exact = TRUE)
  att <- fun(att, ...)
  attr(x, which) <- att
  x
}

#' Render Attribute for Data Frame
#'
#' Renders Attribute for data.frame.
#' Calls \code{\link{render_attr}}
#' for each column.
#'
#'
#' @param x data.frame
#' @param which attribute name (exact match); see \code{\link{attr}}
#' @param fun function or function name to process attribute
#' @param ... passed arguments
#' @export
#' @return data.frame
#' @examples
#' library(magrittr)
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' file %>%
#' decorate %>%
#' explicit_guide %>%
#' append_units %>%
#' render_attr('label', toupper) %>%
#' as_yamlet

render_attr.data.frame <- function(x, which, fun = concatenate, ...){
  x[] <- lapply(x, render_attr, which = which, fun = fun, ...)
  x
}

