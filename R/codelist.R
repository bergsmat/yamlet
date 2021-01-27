globalVariables(c('.nms','.vls','values'))
#' Create a Codelist
#'
#' Creates a codelist.  Generic, with method \code{\link{codelist.data.frame}}
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @return see methods
#' @family codelist
#' @examples
#' example(codelist.data.frame)
codelist <- function(x, ...)UseMethod('codelist')

#' Create a Codelist for Data Frame
#'
#' Creates a codelist for data.frame.
#' Assuming one-to-one correspondence of \code{names}
#' and \code{values} (error otherwise),
#' returns a named list of unique values, in
#' order given by \code{as.factor(names)}.
#'
#' @param x data.frame
#' @param names unquoted column name supplying list names
#' @param values unquoted colum name supplying list values
#' @param ... ignored arguments
#' @export
#' @importFrom dplyr mutate %>% rename arrange
#' @importFrom rlang enquo
#' @keywords internal
#' @return named list
#' @family codelist
#' @examples
#' library(magrittr)
#'
#' # Suppose we have a numeric and a character version of treatment:
#'
#' x <- data.frame(
#'   TRTN =  c(        0,      1,            2,         0),
#'   TRTC =  c('placebo', 'drug', 'comparitor', 'placebo')
#' )
#'
#' x
#'
#' # Further, we wish to specify a particular display order:
#'
#' x$TRTC <- factor(x$TRTC, levels = c('drug', 'comparitor', 'placebo'))
#'
#' # We can use TRTC as the guide for TRTN:
#'
#' x %<>% modify(TRTN, guide = codelist(x, TRTC, TRTN))
#'
#' x %>% decorations(TRTN)
#'
#'
codelist.data.frame <- function(x, names, values, ...){
  nms <- enquo(names)
  vls <- enquo(values)
  x <- rename(x, .nms = !!nms)
  x <- rename(x, .vls = !!vls)
  x$.nms <- as.factor(x$.nms)
  x <- arrange(x, .nms)
  y <- unique(select(x, .nms, .vls))
  if(nrow(y) != length(unique(y$.vls)))stop('names and values not one-to-one')
  out <- as.list(y$.vls)
  names(out) <- y$.nms
  out
}
#' Create a Codelist by Default
#'
#' Creates a codelist by default.
#' Coerces to factor, and supplies
#' levels as names of corresponding
#' elements.
#'
#'
#' @param x vector or factor
#' @param names optional
#' @param ... passed to \code{\link{factor}}
#' @export
#' @importFrom dplyr mutate %>% rename arrange
#' @importFrom rlang enquo
#' @keywords internal
#' @return list, possibly named
#' @family codelist
#' @examples
#' library(magrittr)
#'
#' # Suppose we have a numeric and a character version of treatment:
#'
#' x <- data.frame(
#'   TRTN =  c(        0,      1,            2,         0),
#'   TRTC =  c('placebo', 'drug', 'comparitor', 'placebo')
#' )
#'
#' x
#'
#' # Further, we wish to specify a particular display order:
#'
#' x$TRTC <- factor(x$TRTC, levels = c('drug', 'comparitor', 'placebo'))
#'
#' # We can use TRTC as the guide for TRTN:
#'
#' x %<>% modify(TRTN, guide = codelist(x, TRTC, TRTN))
#'
#' x %>% decorations(TRTN)
#'
#'
codelist.default <- function(x, levels, ...){
  vls <- factor()
  nms <- enquo(names)
  vls <- enquo(values)
  x <- rename(x, .nms = !!nms)
  x <- rename(x, .vls = !!vls)
  x$.nms <- as.factor(x$.nms)
  x <- arrange(x, .nms)
  y <- unique(select(x, .nms, .vls))
  if(nrow(y) != length(unique(y$.vls)))stop('names and values not one-to-one')
  out <- as.list(y$.vls)
  names(out) <- y$.nms
  out
}
