#' Enforce Isometry
#' 
#' Enforces isometric plot design:  aspect ratio of 1, identical 
#' ranges for x and y axes.
#' @return ggplot_isometric
#' @seealso ggplot_add.ggplot_isometric
#' @export
#' @keywords internal
#' @family isometric
#' @examples
#' library(magrittr)
#' library(ggplot2)
#' data.frame(x = 1:5, y = 3:7) %>%
#' ggplot(aes(x, y)) + geom_point() + isometric()

isometric <- function()structure(list(), class = 'ggplot_isometric')

#' Add Isometry to Plot Object
#' 
#' Adds isometry to plot object.
#' @return gg
#' @seealso isometric
#' @export
#' @keywords internal
#' @importFrom ggplot2 ggplot_add theme
#' @method ggplot_add ggplot_isometric
#' @family isometric
#' @examples
#' example(isometric)
ggplot_add.ggplot_isometric <- function(object, plot, object_name){
  stopifnot('x' %in% names(plot$labels))
  stopifnot('y' %in% names(plot$labels))
  xrange <- range(na.rm = TRUE, plot$data[,plot$labels$x])
  yrange <- range(na.rm = TRUE, plot$data[,plot$labels$y])
  plot <- plot + expand_limits(x = yrange)
  plot <- plot + expand_limits(y = xrange)
  plot <- plot + theme(aspect.ratio = 1)
  plot
}

#' Enforce Symmetry
#' 
#' Enforces symmetric plot design: y axis includes opposites of the range of the data.
#' @return ggplot_symmetric
#' @seealso ggplot_add.ggplot_symmetric
#' @export
#' @keywords internal
#' @family isometric
#' @examples
#' library(magrittr)
#' library(ggplot2)
#' data.frame(x = 1:10, y = c(-2, 5, 0, -1, 4, 0, 1, -3, 3, 0)) %>%
#' ggplot(aes(x, y)) + geom_point() + symmetric()
#' 
symmetric <- function()structure(list(), class = 'ggplot_symmetric')

#' Add Symmetry to Plot Object
#' 
#' Adds y axis symmetry to plot object.
#' @return gg
#' @seealso symmetric
#' @export
#' @keywords internal
#' @importFrom ggplot2 ggplot_add expand_limits
#' @method ggplot_add ggplot_symmetric
#' @family isometric
#' @examples
#' example(symmetric)

ggplot_add.ggplot_symmetric <- function(object, plot, object_name){
  nms <- names(plot$labels)
  stopifnot('y' %in% nms)
  yrange <- range(na.rm = TRUE, plot$data[,plot$labels$y])
  plot <- plot + expand_limits(y = -yrange)
  plot
}

#' @export 
ggplot2::ggplot_add

