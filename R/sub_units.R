#' Place Units Under Label
#'
#' Places units attribute below label attribute.
#' Makes the most sense for figures (\code{style = 'plotmath'})
#' and useful for tables (\code{style = 'latex'}) in combination
#' with \code{\link{alias}}. See also \code{\link{append_units}}.
#'
#' @param x object
#' @param ... passed to \code{\link{append_units}}
#' @param open character to precede units
#' @param close character to follow units
#' @param style one of 'plain', 'latex', or 'plotmath'
#' @param math_open,math_close,label_open,label_close,newline passed to \code{\link{as_latex.spar}} if style = 'latex'
#' @export
#' @keywords internal
#' @family labels
#' @return see methods for \code{\link{append_units}}
#' @examples
#' library(units)
#' library(magrittr)
#' library(dplyr)
#' library(ggplot2)
#' x <- 1:10
#' attr(x, 'label') <- 'acceleration'
#' units(x) <- 'm/s^2'
#' y <- as_units('kg')
#' x %>% attr('label')
#' x %>% sub_units %>% attr('label')
#' x %>% sub_units(style = 'plotmath') %>% attr('label')
#' x %>% sub_units(style = 'plain') %>% attr('label') %>% writeLines
#' y %>% attr('label')
#' y %>% sub_units(style = 'plain') %>% attr('label')
#' x %>% sub_units(style = 'plotmath')
#' x %>% sub_units(style = 'latex')
#'
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' file %>% decorate %>% resolve %>%
#' sub_units(style = 'plotmath') %>%
#' ggplot(data = ., aes(x = time, y = conc, color = Heart)) %>%
#' add(geom_point())

sub_units <- function(
  x,
  ...,
  open = if(style == 'plain') '\n(' else '\\n(',
  close = ')',
  style = 'latex',
  math_open =  "",
  math_close =  "",
  label_open =  "$\\begin{gathered}",
  label_close =  "\\end{gathered}$",
  newline = '\\\\'
)append_units(
  x,
  open = open,
  close = close,
  style = style,
  math_open = math_open,
  math_close = math_close,
  label_open = label_open,
  label_close = label_close,
  newline = newline,
  ...
)
