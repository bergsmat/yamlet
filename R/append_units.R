#' Append Units
#'
#' Appends units attribute to label attribute.
#' Generic, with methods
#' \code{\link{append_units.default}} and
#' \code{\link{append_units.data.frame}}.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family labels
#' @return see methods
#' @examples
#' # see methods
append_units <- function(x, ...)UseMethod('append_units')

#' Append Units By Default
#'
#' Units attribute is wrapped in \code{open} and
#' \code{close}, and appended to label.
#' Label is then coerced using \code{as}
#' and concatenated using \code{\link{concatenate}}.
#'
#' @param x object
#' @param open character to precede units
#' @param close character to follow units
#' @param style one of character, latex, or plotmath
#' @param ... ignored arguments
#' @export
#' @importFrom spork as_spork
#' @importFrom spork as_plotmath
#' @importFrom spork as_latex
#' @importFrom spork concatenate
#' @keywords internal
#' @family labels
#' @return same class as x with named character label of length four having supplied class
#' @examples
#' library(units)
#' library(magrittr)
#' x <- 1:10
#' attr(x, 'label') <- 'acceleration'
#' units(x) <- 'm/s^2'
#' y <- as_units('kg')
#' x %>% attr('label')
#' x %>% append_units %>% attr('label')
#' y %>% attr('label')
#' y %>% append_units %>% attr('label')
#'
#'
append_units.default <- function(
  x,
  open = getOption( 'append_units_open' , ' (' ),
  close = getOption('append_units_close', ')'  ),
  style = getOption('append_units_style','character'),
  ...
){
  stopifnot(style %in% c('character', 'latex','plotmath'))
  lab <- attr(x, 'label')
  unit <- attr(x, 'units')
  if(is.null(lab)) lab <- ''
  if(!is.null(unit)){
    unit <- as.character(unit)
    lab <- c(label = lab, open = open, units = unit, close = close)
  }
  if(style == 'character'){
    lab <- paste(lab, collapse = '')
  }
  if(style == 'latex'){
    lab <- as_spork(as_spork(lab))
    lab <- paste(lab, collapse = '')
    lab <- as_latex(as_spork(lab))
  }
  if(style == 'plotmath'){
    lab <- as_spork(as_spork(lab))
    lab <- paste(lab, collapse = '')
    lab <- as.expression(as_plotmath(as_spork(lab)))
  }

  attr(x, 'label') <- lab
  x
}

#' Append Units for Data Frame.
#'
#' Appends units for data.frame.
#' For finer control, consider applying
#' \code{\link{append_units.default}}
#' to individual columns.
#'
#' @param x data.frame
#' @param ... passed to default method
#' @export
#' @family labels
#' @return data.frame
#' @examples
#' library(magrittr)
#' library(ggplot2)
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' file %>% decorate %>% explicit_guide %>% append_units %>% as_yamlet

append_units.data.frame <- function(x, ...){
  x[] <- lapply(x, append_units, ...)
  x
}

