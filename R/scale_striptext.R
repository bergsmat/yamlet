#' Scale Striptext
#' 
#' Scales striptext font size by number of striptext lines.
#' Relative font size is adjusted on a per-label
#' basis using \code{(1/x)^n}, where 
#' \code{x} is the number of striptext lines and
#' \code{n} is in the interval [0, 1]. 
#' \code{n} is 0.5 by default but can be passed as the argument \code{object}:
#' e.g. \code{plot <- plot + scale_striptext(1)}. 
#' 
#' 
#' @return ggplot_scale_striptext
#' @seealso ggplot_add.ggplot_scale_striptext
#' @export
#' @importFrom ggplot2 element_text rel
#' @keywords internal
#' @family isometric
#' @param object ggplot
#' @param scale length-one numeric on [0, 1]
#' @param ... ignored
#' @examples
#' library(magrittr)
#' library(dplyr)
#' library(ggplot2)
#' 
#'   mtcars %>% 
#'   ggplot(aes(wt, mpg)) + 
#'   geom_point() +
#'   scale_striptext()
#' 
#'   mtcars %>% 
#'   ggplot(aes(wt, mpg)) +
#'   geom_point() +
#'   facet_grid(cyl ~ am + gear) +
#'   scale_striptext()
#' 
#'   mtcars %>% 
#'   ggplot(aes(wt, mpg)) + 
#'   geom_point() +
#'   facet_wrap(. ~ cyl + am + gear) +
#'   scale_striptext()
#' 
#'   mtcars %>% 
#'   ggplot(aes(wt, mpg)) + 
#'   geom_point() +
#'   facet_wrap(
#'     . ~ cyl + am + gear, 
#'     labeller = labeller(
#'       .default = label_both
#'     )
#'   ) +
#'   scale_striptext()
#' 
#'   mtcars %>% 
#'   ggplot(aes(wt, mpg)) + 
#'   geom_point() +
#'   facet_wrap(
#'     . ~ cyl + am + gear, 
#'     labeller =  labeller(
#'       .default = label_both,
#'       .multi_line = FALSE
#'     )
#'   ) +
#'   scale_striptext()
#' 
#'   mtcars %>% 
#'   ggplot(aes(wt, mpg)) + 
#'   geom_point() +
#'   facet_wrap(
#'     . ~ cyl + am + gear, 
#'     labeller = purrr::partial(
#'       label_both, 
#'       sep = "\n"
#'     )
#'   ) +
#'   scale_striptext()
#' 
#' p <- mtcars %>% 
#'   mutate(cyl = paste('cylinders:', cyl)) %>%
#'   mutate(gear = paste('gears:', gear)) %>%
#'   mutate(am = paste('transmission:', am))%>%
#'   ggplot(aes(wt, mpg)) + 
#'   geom_point() +
#'   facet_wrap(
#'     . ~ cyl + am + gear, 
#'     labeller = labeller(
#'       .default = label_wrap_gen(10)
#'     )
#'   ) 
#'   p
#'   p + scale_striptext(0)
#'   p + scale_striptext(0.5)
#'   p + scale_striptext(1)
#' 
#'   mtcars %>% 
#'   mutate(cyl =  paste(sep = '\n', 'cylinders', cyl)) %>%
#'   mutate(gear = paste(sep = '\n', 'gears', gear)) %>%
#'   mutate(am =   paste(sep = '\n', 'transmission', am))%>%
#'   ggplot(aes(wt, mpg)) + 
#'   geom_point() +
#'   facet_wrap(
#'     . ~ cyl + am + gear, 
#'     labeller = labeller(
#'       .default = label_wrap_gen(10)
#'     )
#'   ) + 
#'   scale_striptext()
#'
#'   mtcars %>% 
#'   mutate(cyl =  paste(sep = ':', 'cylinders', cyl)) %>%
#'   mutate(gear = paste(sep = ':', 'gears', gear)) %>%
#'   mutate(am =   paste(sep = ':', 'transmission', am))%>%
#'   ggplot(aes(wt, mpg)) + 
#'   geom_point() +
#'   facet_wrap(
#'     . ~ paste(sep = '\n', cyl, am, gear), 
#'   ) + 
#'   scale_striptext()
#'

scale_striptext <- function(
    object = getOption('ggplot_scale_striptext', 0.5), 
    ...
  ){
  scale <- object
  stopifnot(is.numeric(scale))
  stopifnot(length(scale) == 1)
  stopifnot(scale >= 0)
  stopifnot(scale <= 1)
  structure(scale, class = 'ggplot_scale_striptext')
}

#' Adjust Striptext Font Size
#' 
#' Adjust striptext font size for class 'ggplot'.
#' @return gg
#' @export
#' @param object length-one non-negative real number controling severity of scale adjustment
#' @param plot ggplot
#' @param object_name unused
#' @keywords internal
#' @importFrom ggplot2 ggplot_add theme
#' @importFrom rlang sym
#' @method ggplot_add ggplot_scale_striptext
#' @family isometric
#' @examples
#' example(scale_striptext)
ggplot_add.ggplot_scale_striptext <- function(
    object, 
    plot, 
    object_name
  ){
  # lines <- lines(striptext(plot))
  # lines[lines == 0] <- 1
  # scale = (1/lines)^object
  plot = plot + theme(
    strip.text = element_text_refit(scale = object)
  )
  return(plot)
}

#' Create Grob for Refit Text
#' 
#' Creates a grob for refit text. Divides \code{size} by number 
#' of lines, and raises the result to the power of \code{scale}.
#' @importFrom stringr str_count
#' @importFrom ggplot2 element_grob
#' @importFrom rlang %||%
#' @export
#' @return grob
#' @family isometric
#' @keywords internal
#' @param element passed to \code{\link{ggplot2::element_grob}}
#' @param label passed to \code{\link{ggplot2::element_grob}}
#' @param size passed to \code{\link{ggplot2::element_grob}}
#' @param scale a numeric between 0 and 1, inclusive
element_grob.element_text_refit <- function(
    element,
    label = '',
    size = NULL,
    scale = getOption('element_text_refit_scale', 0.5),
    ...
){
  # following Teun van den Brand, see:
  # https://github.com/tidyverse/ggplot2/issues/4979
  n_lines <- stringr::str_count(label, pattern = '\n') + 1
  size <- rep_len(size %||% element$size, length(n_lines)) / n_lines
  stopifnot(
    length(scale) == 1,
    is.finite(scale),
    scale >= 0,
    scale <= 1
  )
  size <- size^scale # added by TTB
  class(element) <- setdiff(class(element), 'element_text_refit')
  element_grob(element, label = label, size = size, ...)
}

#' Create element_text_refit
#' 
#' Creates instance of class 'element_text_refit'.
#' See also \code{\link{element_grob.element_text_refit}}.
#' @export
#' @return element_text_refit
#' @param ... passed to \code{\link{ggplot2::element_text}}
#' @importFrom ggplot2 element_text
element_text_refit <- function(...,  scale){
  elem <- element_text(...)
  class(elem) <- c('element_text_refit', class(elem))
  elem
}

