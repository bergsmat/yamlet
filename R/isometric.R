globalVariables(c('_yamlet_ymin','_yamlet_ymax','_yamlet_xmin','_yamlet_xmax'))
#' Enforce Isometry
#' 
#' Enforces isometric plot design:  aspect ratio of 1, identical 
#' ranges for x and y axes. Can be used meaningfully with
#' \code{+ facet_wrap(scales = 'free' ...)}.
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
#' @importFrom ggplot2 ggplot_add theme geom_blank aes
#' @importFrom rlang sym
#' @method ggplot_add ggplot_isometric
#' @family isometric
#' @examples
#' example(isometric)
ggplot_add.ggplot_isometric <- function(object, plot, object_name, ...){
  # https://stackoverflow.com/questions/42588238/setting-individual-y-axis-limits-with-facet-wrap-not-with-scales-free-y
  theLabels <- get_labs(plot)
  stopifnot('x' %in% names(theLabels))
  stopifnot('y' %in% names(theLabels))
  wrap_facet <- plot$facet$params$facets
  grid_facet_col <- names(plot$facet$params$rows)
  grid_facet_row <- names(plot$facet$params$cols)
  grid_facets <- c(grid_facet_col, grid_facet_row)
  facets <- character(0)
  if(!is.null(wrap_facet)){
    plot$data <-  group_by(plot$data, !!!wrap_facet)
  }
  if(!is.null(grid_facets)){
    plot$data <- group_by(plot$data, !!!sapply(grid_facets, sym))
  }
  # calculate x,y min,max by group if any
  # https://stackoverflow.com/questions/46131829/unquote-the-variable-name-on-the-right-side-of-mutate-function-in-dplyr
  plot$data <- mutate(plot$data, `_yamlet_ymin` = min(na.rm = TRUE, !!rlang::sym(theLabels$y)))
  plot$data <- mutate(plot$data, `_yamlet_ymax` = max(na.rm = TRUE, !!rlang::sym(theLabels$y)))
  plot$data <- mutate(plot$data, `_yamlet_xmin` = min(na.rm = TRUE, !!rlang::sym(theLabels$x)))
  plot$data <- mutate(plot$data, `_yamlet_xmax` = max(na.rm = TRUE, !!rlang::sym(theLabels$x)))
 
  plot <- plot + geom_blank(aes(y = `_yamlet_xmin`))
  plot <- plot + geom_blank(aes(y = `_yamlet_xmax`))
  plot <- plot + geom_blank(aes(x = `_yamlet_ymin`))
  plot <- plot + geom_blank(aes(x = `_yamlet_ymax`))
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

ggplot_add.ggplot_symmetric <- function(object, plot, object_name, ...){
  theLabels <- get_labs(plot)
  nms <- names(theLabels)
  stopifnot('y' %in% nms)
  yrange <- range(na.rm = TRUE, plot$data[,theLabels$y])
  plot <- plot + expand_limits(y = -yrange)
  plot
}

#' @export 
ggplot2::ggplot_add

