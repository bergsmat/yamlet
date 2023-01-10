#' Append Units
#'
#' Appends units attribute to label attribute.
#' Generic, with methods
#' \code{\link{append_units.default}} and
#' \code{\link{append_units.data.frame}}.
#' For a more general strategy see \code{\link{modify}}.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family deprecated
#' @family labels
#' @return see methods
#' @examples
#' # see methods
append_units <- function(x, ...)UseMethod('append_units')

#' Append Units By Default
#'
#' Units attribute is wrapped in \code{open} and
#' \code{close}, and appended to label.
#' Result is assigned to \code{target} attribute
#' (default: 'label').
#' If style is 'latex' or 'plotmath',
#' all elements are treated as spork
#' (\code{\link{as_spork}}) and coerced
#' to canonical form before concatenation.
#'
#'
#' @param x object
#' @param ... passed to \code{\link{as_latex}}, \code{\link{as_plotmath}}
#' @param open character to precede units
#' @param close character to follow units
#' @param style one of 'plain', 'latex', or 'plotmath'
#' @param target attribute name for appended result
#' @export
#' @importFrom spork as_spork
#' @importFrom spork as_plotmath
#' @importFrom spork as_latex
#' @importFrom spork plotmathToken
#' @importFrom spork latexToken
#' @keywords internal
#' @family labels
#' @return same class as x, with sub-class 'latex' or 'plotmath' depending on \code{style}
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
#' x %>% append_units(style = 'plain')
#' x %>% append_units(style = 'plotmath')
#' x %>% append_units(style = 'latex')
#'
#'
append_units.default <- function(
  x,
  ...,
  open = getOption( 'yamlet_append_units_open' , ' (' ),
  close = getOption('yamlet_append_units_close', ')'  ),
  style = getOption('yamlet_append_units_style','plain'),
  target = getOption('yamlet_append_units_target', 'label')

){
  stopifnot(style %in% c('plain', 'latex','plotmath'))
  lab <- attr(x, 'label')
  unit <- attr(x, 'units')
  unit <- as.character(unit) 
  # coerces symbolic_units nicely, without damaging count of non-singular units
  # but drops names
  if(!inherits(attr(x, 'units'),'symbolic_units')){
    names(unit) <- names(attr(x, 'units'))
  }
  if(is.null(lab)) lab <- ''
  nms <- names(lab)
  lab <- as.list(lab)
  if(
    length(unit)!= length(lab) &
    length(unit) > 1 # zero or 1 is fine
  )warning('length of units does not match length of labels')
  if(!identical(names(lab), names(unit)))warning('names of units do not match names of labels')
  unit <- rep(unit, length(lab))
  open <- rep(open, length(lab))
  close <- rep(close, length(lab))
  
  if(length(unit)){ # can't test for NULL because as.character(NULL) above is not NULL
    # unit <- as.character(unit)
    for(i in seq_along(lab)){
      lab[[i]] <- c(
        label = lab[[i]], 
        open = open[[i]], 
        units = unit[[i]], 
        close = close[[i]]
      )
    }
  }
  if(style == 'plain'){
    for(i in seq_along(lab)){
      lab[[i]] <- paste(lab[[i]], collapse = '')
    }
  }
  if(style == 'latex'){
    for(i in seq_along(lab)){
      lab[[i]] <- as_spork(as_spork(lab[[i]]))
      lab[[i]] <- paste(lab[[i]], collapse = '')
      lab[[i]] <- as_latex(as_spork(lab[[i]]), ...)
    }
  }
  if(style == 'plotmath'){
    for(i in seq_along(lab)){
      lab[[i]] <- as_spork(as_spork(lab[[i]]))
      lab[[i]] <- paste(lab[[i]], collapse = '')
      lab[[i]] <- as.expression(as_plotmath(as_spork(lab[[i]]), ...))
    }
  }
  names(lab) <- nms
  if(length(lab) == 1) lab <- lab[[1]]
  attr(x, target) <- lab
  x
}

#' Append Units for Data Frame
#'
#' Appends units for data.frame.
#' For finer control, consider applying
#' \code{\link{append_units.default}}
#' to individual columns.
#'
#' @param x data.frame
#' @param ... named arguments passed to default method, un-named are columns to alter scope
#' @export
#' @keywords internal
#' @family labels
#' @return data.frame
#' @examples
#' library(magrittr)
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' file %>% decorate %>% explicit_guide %>% append_units %>% decorations(Age, glyco)
#' file %>% decorate %>% explicit_guide %>% append_units(glyco) %>% decorations(Age, glyco)

append_units.data.frame <- function(x, ...){
  vars <- selected(x, ...)
  mods <- named(...)
  for(var in vars){
    # pass only named arguments
    x[[var]] <- do.call(append_units, c(list(x[[var]]),mods))
  }
  #x[] <- lapply(x, append_units, ...)
  x
}
#' @export
spork::latexToken


