#' Footnote Something
#'
#' Footnotes something.
#' Generic, with method \code{\link{footnote.decorated}}.
#' @param x object
#' @param ... passed arguments
#' @family footnote
#' @keywords internal
#' @export
#' @return see methods
#' @examples
#' # see methods
footnote <- function(x, ...)UseMethod('footnote')

#' Footnote Decorated
#'
#' Footnotes a decorated data.frame.
#' Generates a text string that defines
#' column names using label and unit attributes.
#' @param x decorated
#' @param equal character: a symbol suggesting equality between a name and its note
#' @param collapse used to \code{\link{paste}} column-wise footnotes
#' @param ... passed to append_units
#' @family footnote
#' @export
#' @return character
#' @examples
#' library(magrittr)
#' set.seed(0)
#' x <- data.frame(
#'  auc = rnorm(100, mean = 2400, sd = 200),
#'  bmi = rnorm(100, mean = 20, sd = 5),
#'  gen = 0:1
#' )
#' x %<>% decorate('auc: [AUC_0-24, ng*h/mL]')
#' x %<>% decorate('bmi: [Body Mass Index, kg/m^2]')
#' x %<>% decorate('gen: [Gender, [Male: 1, Female: 0]]')
#' footnote(x)

footnote.decorated <- function(x, equal = ':', collapse = '; ', ...){
  x <- append_units(x, ...)
  y <- sapply(x, attr, 'label')
  y <- paste0(names(x), equal, y)
  y <- paste(y, collapse = collapse)
  y
}

#' Create Export Table for Decorated
#'
#' Creates an export table for decorated data.frame
#' by adding a footnote attribute.
#'
#' @param x decorated
#' @param style passed to \code{\link{footnote}}
#' @param ... passed to \code{\link{footnote}} and \code{\link[xtable]{xtable}}
#' @export
#' @importFrom xtable xtable
#' @return class 'decorated', 'xtable','data.frame'
#' @examples
#' library(magrittr)
#' library(xtable)
#' set.seed(0)
#' x <- data.frame(
#'  auc = rnorm(100, mean = 2400, sd = 200),
#'  bmi = rnorm(100, mean = 20, sd = 5),
#'  gen = 0:1
#' )
#' x %<>% decorate('auc: [AUC_0-24, ng*h/mL]')
#' x %<>% decorate('bmi: [Body Mass Index, kg/m^2]')
#' x %<>% decorate('gen: [Gender, [Male: 1, Female: 0]]')
#' y <- xtable(x)
#' attr(y, 'footnote')
#'
xtable.decorated <- function(x,style = 'latex', ...){
  y <- xtable(data.frame(x), ...)
  class(y) <- c('decorated', 'xtable', 'data.frame')
  z <- footnote(x, style = style, ...)
  attr(y, 'footnote') <- z
  y
}

#' Print Decorated
#'
#' Prints a decorated data.frame.
#' If 'xtable' is inherited, supplies a footnote.
#' Experimental.
#'
#' @export
#' @importFrom xtable xtable
#' @importFrom xtable print.xtable
#' @keywords internal
#' @return character
#' @param x decorated
#' @param ... passed to other methods and to \code{\link{footnote}}
#' @examples
#' library(magrittr)
#' library(xtable)
#' set.seed(0)
#' x <- data.frame(
#'  auc = rnorm(4, mean = 2400, sd = 200),
#'  bmi = rnorm(4, mean = 20, sd = 5),
#'  gen = 0:1
#' )
#' x %<>% decorate('auc: [AUC_0-24, ng*h/mL]')
#' x %<>% decorate('bmi: [Body Mass Index, kg/m^2]')
#' x %<>% decorate('gen: [Gender, [Male: 1, Female: 0]]')
#' x %>% resolve
#' x %>% resolve %>% xtable
#'
#'
print.decorated <- function(x, ...){
  if(!inherits(x, 'xtable')){
    NextMethod()
  }else{
    y <- NextMethod(print.results=FALSE, comment = FALSE, ...)
    note <- attr(x,'footnote')
    y <- sub(
      fixed = TRUE,
      '\\end{table}',
      paste(sep = '\n','\n', note, '\\end{table}'),
      y
    )
    cat(y)
   return(invisible())
  }
}




