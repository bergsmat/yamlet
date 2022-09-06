#' Scale Striptext
#' 
#' Scales striptext font size by number of striptext lines.
#' Relative font size is adjusted on a per-axis
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
#'   
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
  lines <- lines(striptext(plot))
  lines[lines == 0] <- 1
  scale = (1/lines)^object
  plot = plot + theme(
    strip.text.x = element_text(size = rel(scale[[1]])),
    strip.text.y = element_text(size = rel(scale[[2]]))
  )
  return(plot)
}

