#' Extract Striptext
#' 
#' Extracts striptext.
#' Generic, with method \code{\link{striptext.ggplot}}.
#' 
#' @export
#' @param x object of dispatch
#' @param ... passed arguments
#' @keywords internal
#' @return list with class 'striptext'
#' @examples
#' library(magrittr)
#' library(dplyr)
#' library(ggplot2)
#' (
#'   mtcars %>% 
#'   ggplot(aes(wt, mpg)) + 
#'   geom_point()
#' ) %>% striptext
#' 
#' (
#'   mtcars %>% 
#'   ggplot(aes(wt, mpg)) +
#'   geom_point() +
#'   facet_grid(cyl ~ am + gear)
#' ) %>% striptext
#' 
#' (
#'   mtcars %>% 
#'   ggplot(aes(wt, mpg)) + 
#'   geom_point() +
#'   facet_wrap(. ~ cyl + am + gear)
#' ) %>% striptext
#' 
#' (
#'   mtcars %>% 
#'   ggplot(aes(wt, mpg)) + 
#'   geom_point() +
#'   facet_wrap(
#'     . ~ cyl + am + gear, 
#'     labeller = labeller(
#'       .default = label_both
#'     )
#'   )
#' ) %>% striptext
#' 
#' (
#'   mtcars %>% 
#'   ggplot(aes(wt, mpg)) + 
#'   geom_point() +
#'   facet_wrap(
#'    . ~ cyl + am + gear, 
#'    labeller =  labeller(
#'      .default = label_both,
#'      .multi_line = FALSE
#'    )
#'  )
#' ) %>% striptext
#' 
#' (
#'   mtcars %>% 
#'   ggplot(aes(wt, mpg)) + 
#'   geom_point() +
#'   facet_wrap(
#'    . ~ cyl + am + gear, 
#'    labeller = purrr::partial(
#'      label_both, 
#'      sep = "\n"
#'    )
#'  )
#' ) %>% striptext
#' 
#' (
#'   mtcars %>% 
#'   mutate(cyl = paste('cylinders:', cyl)) %>%
#'   mutate(gear = paste('gears:', gear)) %>%
#'   mutate(am = paste('transmission:', am))%>%
#'   ggplot(aes(wt, mpg)) + 
#'   geom_point() +
#'   facet_wrap(
#'     . ~ cyl + am + gear, 
#'     labeller = labeller(
#'       .default = label_wrap_gen(10),
#'       .multi_line = TRUE
#'     )
#'   )
#' ) %>% striptext
#'
#' (
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
#'   )
#' ) %>% striptext
#'
#' (
#'   mtcars %>% 
#'   mutate(cyl =  paste(sep = ':', 'cylinders', cyl)) %>%
#'   mutate(gear = paste(sep = ':', 'gears', gear)) %>%
#'   mutate(am =   paste(sep = ':', 'transmission', am))%>%
#'   ggplot(aes(wt, mpg)) + 
#'   geom_point() +
#'   facet_wrap(
#'     . ~ paste(sep = '\n', cyl, am, gear), 
#'   )
#' ) %>% striptext
#'
striptext <- function(x, ...)UseMethod('striptext')

#' Extract Striptext from ggplot
#' 
#' Extracts striptext from ggplot.
#' Generic, with method \code{\link{striptext.ggplot}}.
#' 
#' @export
#' @importFrom ggplot2 ggplotGrob
#' @param x ggplot
#' @param ... ignored
#' @keywords internal
#' @return list
#' @examples
#' example(striptext)
striptext.ggplot <- function(x, ...) {
  # https://github.com/tidyverse/ggplot2/issues/4979
  suppressMessages(
    trace(
      what = ggplot2:::build_strip,
      print = FALSE,
      exit = substitute(
        options(
          yamlet_strip_trace = c(
            getOption('yamlet_strip_trace', default = list()),
          list(labels)
          )
        )
      )
    )
  )
  invisible(ggplotGrob(x))
  suppressMessages(untrace(ggplot2:::build_strip))
  out <- getOption('yamlet_strip_trace', default = list())
  options(yamlet_strip_trace = NULL)
  class(out) <- 'striptext'
  out
}
