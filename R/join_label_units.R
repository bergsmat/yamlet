#' Combine Label and Units
#'
#' Combines label attribute and units attribute
#' as label attribute.  Generic, with default
#' method \code{\link{join_label_units.default}}.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family labels
#' @return see methods
#' @examples
#' example(join_label_units.default)
join_label_units <- function(x, ...)UseMethod('join_label_units')

#' Combine Label and Units By Default
#'
#' Combines label attribute and units attribute
#' as label attribute by default. Units attribute,
#' if present, is pasted to label.  If label is non-null,
#' units is wrapped in \code{wrap}.
#'
#' @param x object
#' @param wrap character to wrap units (will be coerced to length two)
#' @param ... ignored arguments
#' @export
#' @importFrom spork as_spork
#' @importFrom spork as_plotmath
#' @keywords internal
#' @family labels
#' @return same class as x with updated label attribute
#' @examples
#' library(units)
#' library(magrittr)
#' x <- 1:10
#' attr(x, 'label') <- 'acceleration'
#' units(x) <- 'm/s^2'
#' y <- as_units('kg')
#' x %>% attr('label')
#' x %>% join_label_units %>% attr('label')
#' y %>% attr('label')
#' y %>% join_label_units %>% attr('label')
#'
#'
join_label_units.default <- function(x, wrap = c(' (',')'), ...){
  lab <- attr(x, 'label')
  unit <- attr(x, 'units')
  if(is.null(unit)) return(x)
  if(!is.null(lab)){
    stopifnot(is.character(wrap))
    rep(wrap, length.out = 2)
    unit <- paste0(wrap[[1]], unit, wrap[[2]])
  }
  lab <- paste0(lab, unit)
  attr(x, 'label') <- lab
  x
}

#' Combine Labels and Units for Data Frame.
#'
#' Combines labels and units for data.frame.
#' For finer control, consider applying
#' \code{\link{join_label_units.default}}
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
#' file %>%
#'  decorate %>%
#'  filter(!is.na(conc)) %>%
#'  ggready(parse = FALSE) %>%
#'  ggplot(aes(x = time, y = conc, color = Heart)) +
#'  geom_point()
#'
join_label_units.data.frame <- function(x, ...){
  x[] <- lapply(x, join_label_units, ...)
  x
}
#' Convert Labels and Units to Expression for Data Frame.
#'
#' Converts labels and units to expression for data.frame.
#' For finer control, consider applying
#' \code{\link{express_label_units.default}}
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
#' file %>%
#'  decorate %>%
#'  filter(!is.na(conc)) %>%
#'  ggready(parse = TRUE) %>%
#'  ggplot(aes(x = time, y = conc, color = Heart)) +
#'  geom_point()

express_label_units.data.frame <- function(x, ...){
  x[] <- lapply(x, express_label_units, ...)
  x
}

#' Convert Label and Units to Expression
#'
#' Converts Label and Units to Expression.
#' Generic, with default method.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family labels
#' @return see methods
#' @examples
#' example(express_label.default)
express_label_units <- function(x, ...)UseMethod('express_label_units')
#' Convert Label to Expression
#'
#' Converts Label to Expression.
#' Joins with units by default, and coerces to
#' spork, plotmath, and expression.
#'
#' @param x object
#' @param wrap character to wrap units (will be coerced to length two)
#' @param ... passed arguments
#' @export
#' @importFrom spork as_plotmath
#' @importFrom spork plotmathToken
#' @family labels
#' @return like x with label attribute of class 'expression'
#' @examples
#' library(units)
#' library(magrittr)
#' x <- 1:10
#' attr(x, 'label') <- 'acceleration'
#' units(x) <- 'm/s^2' # using units package
#' y <- as_units('kg') # using units package
#' x %>% attr('label')
#' x %>% express_label_units %>% attr('label')
#' y %>% attr('label')
#' y %>% express_label_units %>% attr('label')
#' express_label_units(2)
#' z <- 1:10
#' attr(z, 'label') <- 'joules'
#' attr(z, 'units') <- 'kg.m^2./s^2' # vanilla units
#' express_label_units(z, wrap = c(' [',']'))
#' a <- 3
#' attr(a, 'label') <- 'C_max_ss'
#' attr(a, 'units') <- 'ng/mL_o'
#' a %>% express_label_units %>% attr('label')


express_label_units.default <- function(x, wrap = c(' (',')'), ...){
  stopifnot(is.character(wrap))
  if(length(wrap) == 0) wrap <- ''
  rep(wrap, length.out = 2)
  lab <- attr(x, 'label')
  if(!is.null(lab)) lab <- as_plotmath(as_spork(lab))
  unit <- attr(x, 'units')
  if(!is.null(unit)) unit <- as_plotmath(as_spork(unit))
  open <- as_plotmath(as_spork(wrap[[1]]))
  close <- as_plotmath(as_spork(wrap[[2]]))
  if(!is.null(unit) & !is.null(lab))lab <- c(lab, open, unit, close)
  lab <- paste(lab, collapse = '*')
  # restore dropped class
  class(lab) <- union('plotmath', class(lab))
  lab <- as.expression(lab)
  attr(x, 'label') <- lab
  x
}

#' @export
spork::plotmathToken
