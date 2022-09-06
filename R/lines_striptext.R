#' Count Lines of Striptext
#' 
#' Counts lines of striptext. Returns length two integer
#' indicating the highest number of lines encountered, per axis.
#' Line breaks are literal newlines, but also implied
#' where multiple columns exist per list element.
#' See also \code{\link{striptext}}.
#' 
#' @export
#' @param x list with class 'striptext'
#' @param ... passed arguments
#' @keywords internal
#' @return length one integer
#' @examples
#' library(magrittr)
#' library(dplyr)
#' library(ggplot2)
#' (
#'   mtcars %>% 
#'   ggplot(aes(wt, mpg)) + 
#'   geom_point()
#' ) %>% striptext %>% lines
#' 
#' (
#'   mtcars %>% 
#'   ggplot(aes(wt, mpg)) +
#'   geom_point() +
#'   facet_grid(cyl ~ am + gear)
#' ) %>% striptext %>% lines
#' 
#' (
#'   mtcars %>% 
#'   ggplot(aes(wt, mpg)) + 
#'   geom_point() +
#'   facet_wrap(. ~ cyl + am + gear)
#' ) %>% striptext %>% lines
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
#' ) %>% striptext %>% lines
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
#' ) %>% striptext %>% lines
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
#' ) %>% striptext %>% lines
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
#' ) %>% striptext %>% lines
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
#' ) %>% striptext %>% lines
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
#' ) %>% striptext %>% lines
#'

lines.striptext <- function(x, ...) {
  # each striptext is a list
  # sometimes one, sometimes two members
  # each member may have arbitrary columns.
  # we want, for each column, the largest number
  # of fragments when splitting by newline.
  # then sum these across columns.
  
  if(!length(x))return(c(0L, 0L))
  splits <- function(x)strsplit(x, '\n', fixed = TRUE)
  column <- function(x)max(sapply(splits(x), length))
  member <- function(x)sum(apply(x, MARGIN = 2, FUN = column))
  element<- function(x)sapply(x, member)
  out <- element(x, ...)
  stopifnot(length(out) == 2)
  return(out)
}
