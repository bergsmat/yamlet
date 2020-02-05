#' Coerce to Wiki Symbol
#'
#' Coerces to class 'wikisymbol'. Generic,
#' with method \code{\link{as_wikisymbol.character}}.
#' A Wiki symbol is simple text with arbitrarily
#' nested subscript (\code{_}) and superscript
#' (\code{^}) groupings.
#'
#' Wiki symbol is intended as a compact syntax
#' that abstracts across differences in
#' plotmath and latex.  The main purpose
#' is to handle nested subscripts and superscripts,
#' but bare words and special symbols are not
#' disallowed. Support is defined by
#' \code{\link{as_plotmath}} and
#' \code{\link{as_latex}}.
#' Additional rules:
#'
#' * Use dot (\code{.}) to explicitly terminate a grouping.
#' * Use asterisk(\code{*}) to suggest multiplication.
#' * Escape specials with a backslash.
#' * Trailing dots need not be supplied.
#' * Leading/trailing whitespace is ignored.
#' * Internal white space should be respected.
#' * Tab character is not allowed.
#'
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family wikisymbol
#' @return wikisymbol
#' @md
#' @examples
#' library(magrittr)
#' library(dplyr)
#' library(ggplot2)
#' library(latexpdf)
#'
#' data.frame(y=1:10, x=1:10) %>%
#' decorate("x: Omega joule^\\*. ~1 kg*m^2./s^2. [%]") %>%
#' mutate(x = structure(x, label = x %>% attr('label') %>%
#' as_wikisymbol %>%
#' as_plotmath %>%
#' as.expression)) %>%
#' ggplot(aes(x, y))
#'
#' data.frame(y=1:10, x=1:10) %>%
#' decorate("x: gravitational force - gamma (kg\\.m/s^2.)") %>%
#' mutate(x = structure(x, label = x %>% attr('label') %>%
#' as_wikisymbol %>%
#' as_plotmath %>%
#' as.expression)) %>%
#' ggplot(aes(x, y))
#'
#' path <- tempfile()
#' data.frame(
#'  stringsAsFactors = FALSE,
#'  wikisymbol = c(
#'    'Omega joule^\\*. ~1 kg*m^2./s^2. [%]',
#'    'gravitational force - gamma (kg\\.m/s^2.)'
#'  )
#') %>%
#'  transmute(
#'    latex = wikisymbol %>%
#'    as_wikisymbol %>%
#'    as_latex
#'  ) %>%
#'  as.pdf(reserve = FALSE, wider = -70)
as_wikisymbol <- function(x, ...)UseMethod('as_wikisymbol')

#' Coerce Character to Wiki Symbol
#'
#' Coerces character to class 'wikisymbol'.
#' See description for \code{\link{as_wikisymbol}}.
#'
#' @param x character
#' @param ... ignored arguments
#' @export
#' @family wikisymbol
#' @return wikisymbol
#' @examples
#' as_wikisymbol('V_c./F')
as_wikisymbol.character <- function(x, ...){
  if(any(grepl('\t', x))){
    stop('wikisymbol cannot contain tabs')
  }
  class(x) <- union('wikisymbol', class(x))
  x
}

#' Coerce Factor to Wiki Symbol
#'
#' Coerces factor to class 'wikisymbol'
#' by converting to character and calling
#' \code{\link{as_wikisymbol}}.
#'
#' @param x factor
#' @param ... ignored arguments
#' @export
#' @keywords internal
#' @family wikisymbol
#' @return wikisymbol
#' @examples
#' as_wikisymbol(as.factor('V_c./F'))
as_wikisymbol.factor <- function(x, ...)as_wikisymbol(as.character(x), ...)

#' Coerce to Plotmath
#'
#' Coerce to plotmath.  Generic, with method
#' \code{\link{as_plotmath.wikisymbol}}.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family wikisymbol
#' @return plotmath
#' @examples
#' example(as_plotmath.wikisymbol)
as_plotmath <- function(x, ...)UseMethod('as_plotmath')

#' Coerce to Latex
#'
#' Coerce to latex.  Generic, with method
#' \code{\link{as_latex.wikisymbol}}.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family wikisymbol
#' @return latex
#' @examples
#' example(as_latex.wikisymbol)
as_latex <- function(x, ...)UseMethod('as_latex')

#' Convert Wiki Symbol to Plotmath
#'
#' Converts wiki symbol to plotmath. See '?plotmath'.
#' Vectorized version of \code{\link{wikisym2plotmath_}}.
#'
#' @export
#' @param x wikisymbol
#' @param ... ignored
#' @return plotmath
#' @family wikisymbol
#' @examples
#' x <- c(
#'   'V_c./F',
#'   'AUC_ss',
#'   'C_max_ss',
#'   'var^eta_j'
#' )
#' x <- as_wikisymbol(x)
#' as_plotmath(x)
#' as_plotmath(as_wikisymbol('gravitational force (kg\\.m/s^2.)'))
as_plotmath.wikisymbol <- function(x, ...){
  y <- sapply(x, wikisym2plotmath_ , USE.NAMES = F)
  if(length(y) == 0) y <- character(0)
  class(y) <- union('plotmath', class(y))
  y
}
#' Convert Wiki Symbol to Latex
#'
#' Converts wiki symbol to latex.
#' Vectorized version of \code{\link{wikisym2latex_}}.
#'
#' @export
#' @param x wikisymbol
#' @param ... ignored
#' @return latex
#' @family wikisymbol
#' @examples
#' x <- c(
#'   'V_c./F',
#'   'AUC_ss',
#'   'C_max_ss',
#'   'var^eta_j'
#' )
#' x <- as_wikisymbol(x)
#' as_latex(x)
#' as_latex(as_wikisymbol('gravitational force (kg\\.m/s^2.)'))
as_latex.wikisymbol <- function(x, ...){
  y <- sapply(x, wikisym2latex_ , USE.NAMES = F)
  if(length(y) == 0) y <- character(0)
  class(y) <- union('latex', class(y))
  y
}

#' Coerce Plotmath to Expression
#'
#' Coerces plotath to expression by parsing as text.
#' @export
#' @keywords internal
#' @family wikisymbol
#' @param x plotmath
#' @param ... ignored arguments
#' @return expression
#' @examples
#' x <- c(
#'   'V_c./F',
#'   'AUC_ss',
#'   'C_max_ss',
#'   'var^eta_j'
#' )
#' x <- as_wikisymbol(x)
#' x <- as_plotmath(x)
#' x
#' as.expression(x)[[4]]
#' as.expression(x[[4]])
#' class(as.expression(x))
#' lapply(as.expression(x), class)
#' as.expression(as_plotmath(as_wikisymbol('V_c./F')))
#' as.expression(as_plotmath(as_wikisymbol(character(0))))
#' library(magrittr)
#' 'gravitational force (kg\\.m/s^2.)' %>%
#'   as_wikisymbol %>%
#'   as_plotmath %>%
#'   as_expression -> label
#'   label
#'
as.expression.plotmath <- function(x, ...)parse(text = x)


# parse_one <- function(x){
#   stopifnot(length(x) == 1)
#   stopifnot(inherits(x, 'character'))
#   y <- parse(text = x)
#   y
# }

#' Subset Wiki Symbol
#'
#' Subsets wikisymbol, retaining class.
#' @param x wikisymbol
#' @param ... passed to next method
#' @export
#' @keywords internal
#' @family wikisymbol
#' @return wikisymbol
#' @examples
#' x <- c(
#'   'V_c./F',
#'   'AUC_ss',
#'   'C_max_ss',
#'   'var^eta_j'
#' )
#' x <- as_wikisymbol(x)
#' class(x)
#' class(x[1])
`[.wikisymbol` <- function(x, ...){
  y <- NextMethod()
  # contrasts and levels will have been handled
  class(y) <- union('wikisymbol', class(y))
  y
}
#' Element-select Wiki Symbol
#'
#' Element-selects wikisymbol, retaining class.
#' @param x wikisymbol
#' @param ... passed to next method
#' @export
#' @keywords internal
#' @family wikisymbol
#' @return wikisymbol
#' @examples
#' x <- c(
#'   'V_c./F',
#'   'AUC_ss',
#'   'C_max_ss',
#'   'var^eta_j'
#' )
#' x <- as_wikisymbol(x)
#' class(x)
#' class(x[[1]])
`[[.wikisymbol` <- function(x, ...){
  y <- NextMethod()
  # contrasts and levels will have been handled
  class(y) <- union('wikisymbol', class(y))
  y
}

#' Subset Plotmath
#'
#' Subsets plotmath, retaining class.
#' @param x plotmath
#' @param ... passed to next method
#' @export
#' @keywords internal
#' @family wikisymbol
#' @return plotmath
#' @examples
#' x <- c(
#'   'V_c./F',
#'   'AUC_ss',
#'   'C_max_ss',
#'   'var^eta_j'
#' )
#' x <- as_plotmath(as_wikisymbol(x))
#' class(x)
#' class(x[1])
`[.plotmath` <- function(x, ...){
  y <- NextMethod()
  # contrasts and levels will have been handled
  class(y) <- union('plotmath', class(y))
  y
}
#' Element-select Plotmath
#'
#' Element-selects plotmath, retaining class.
#' @param x plotmath
#' @param ... passed to next method
#' @export
#' @keywords internal
#' @family wikisymbol
#' @return plotmath
#' @examples
#' x <- c(
#'   'V_c./F',
#'   'AUC_ss',
#'   'C_max_ss',
#'   'var^eta_j'
#' )
#' x <- as_plotmath(as_wikisymbol(x))
#' class(x)
#' class(x[[1]])
`[[.plotmath` <- function(x, ...){
  y <- NextMethod()
  # contrasts and levels will have been handled
  class(y) <- union('plotmath', class(y))
  y
}

##################

#' Subset Latex
#'
#' Subsets latex, retaining class.
#' @param x latex
#' @param ... passed to next method
#' @export
#' @keywords internal
#' @family wikisymbol
#' @return latex
#' @examples
#' x <- c(
#'   'V_c./F',
#'   'AUC_ss',
#'   'C_max_ss',
#'   'var^eta_j'
#' )
#' x <- as_latex(as_wikisymbol(x))
#' class(x)
#' class(x[1])
`[.latex` <- function(x, ...){
  y <- NextMethod()
  # contrasts and levels will have been handled
  class(y) <- union('latex', class(y))
  y
}
#' Element-select Latex
#'
#' Element-selects latex, retaining class.
#' @param x latex
#' @param ... passed to next method
#' @export
#' @keywords internal
#' @family wikisymbol
#' @return latex
#' @examples
#' x <- c(
#'   'V_c./F',
#'   'AUC_ss',
#'   'C_max_ss',
#'   'var^eta_j'
#' )
#' x <- as_latex(as_wikisymbol(x))
#' class(x)
#' class(x[[1]])
`[[.latex` <- function(x, ...){
  y <- NextMethod()
  # contrasts and levels will have been handled
  class(y) <- union('latex', class(y))
  y
}

#' Coerce Symbolic Units to Wiki Symbol.
#'
#' Coerces symbolic units to wikisymbol.  A literal dot
#' means different things in wikisymbol vs. units,
#' and there may be some other subtleties as well.
#' @param x units; see \code{\link[units]{as_units}}
#' @param ... ignored arguments
#' @export
#' @family wikisymbol
#' @return units
#' @examples
#' library(units)
#' x <- as_units('kg.m/s^2')
#' names(attributes(x))
#' y <- attr(x,'units')
#' class(y)
#' as.character(y)
#' as.character(attr(x, 'units'))
#' as_wikisymbol(y)
#' library(magrittr)
#' 'kg.m^2/s^2' %>% as_units %>% attr('units') %>% as_wikisymbol
as_wikisymbol.symbolic_units <- function(x, ...){
  y <- as.character(x)
  y <- gsub('\\.','*',y) # \u22c5 https://en.wikipedia.org/wiki/Interpunct
  y <- gsub('\\^([0-9])+','^\\1.',y)
  y <- as_wikisymbol(y)
  y
}

