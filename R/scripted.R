#' Render Scripted Attributes of Indicated Components
#'
#' Renders the scripted attributes of indicated components.
#' Generic: an alias for \code{\link{enscript}}.
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @return see methods
scripted <- function(x, ...)UseMethod('enscript')
