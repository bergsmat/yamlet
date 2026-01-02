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
#' @param expand amount of scale expansion to allow, e.g. 0.05 (default 0 for best fidelity)
#' @examples
#' library(magrittr)
#' library(ggplot2)
#' data.frame(x = 1:5, y = 3:7) %>%
#' ggplot(aes(x, y)) + geom_point() + isometric()

isometric <- function(expand = 0)structure(list(expand = expand), class = 'ggplot_isometric')

#' Add Isometry to Plot Object
#' 
#' Adds isometry to plot object.
#' @return gg
#' @seealso isometric
#' @export
#' @keywords internal
#' @importFrom ggplot2 ggplot_add theme geom_blank aes ggplot_build coord_fixed
#' @importFrom rlang sym
#' @method ggplot_add ggplot_isometric
#' @family isometric
#' @examples
#' example(isometric)
ggplot_add.ggplot_isometric <- function(object, plot, object_name, ...) {
  # as suggested by chatgpt 5
  # Build the plot to learn trained panel ranges (includes stats like geom_smooth)
  b <- ggplot2::ggplot_build(plot)
  
  # panel_params holds the trained ranges; there is one entry per panel
  pp <- b$layout$panel_params
  
  # helper to extract range across ggplot2 versions
  get_range <- function(panel, axis = c("x", "y")) {
    axis <- match.arg(axis)
    # new-ish ggplot2 uses <axis>.range; older uses range$range
    if (!is.null(panel[[paste0(axis, ".range")]])) {
      panel[[paste0(axis, ".range")]]
    } else if (!is.null(panel[[axis]]$range$range)) {
      panel[[axis]]$range$range
    } else {
      stop("Could not extract trained panel ranges from ggplot build.")
    }
  }
  
  x_ranges <- lapply(pp, get_range, axis = "x")
  y_ranges <- lapply(pp, get_range, axis = "y")
  
  # If facets have free scales, these ranges differ panel-to-panel.
  # Base ggplot2 can’t set coord limits per panel.
  same_x <- length(unique(vapply(x_ranges, paste, "", collapse = ","))) == 1
  same_y <- length(unique(vapply(y_ranges, paste, "", collapse = ","))) == 1
  if (!(same_x && same_y) && length(pp) > 1) {
    stop(
      "isometric(): Facets appear to use free scales (panel ranges differ).\n",
      "Base ggplot2 can't enforce different x/y limits per panel.\n",
      "Options:\n",
      "  * Use fixed scales (the default), or\n",
      "  * Use ggh4x::facetted_pos_scales() to set per-facet limits, or\n",
      "  * Split by facet and combine plots (patchwork/cowplot)."
    )
  }
  
  # Compute one common limit that matches BOTH axes.
  xr <- range(unlist(x_ranges), finite = TRUE)
  yr <- range(unlist(y_ranges), finite = TRUE)
  lim <- range(c(xr, yr), finite = TRUE)
  
  # Apply: fixed aspect + same numeric limits on both axes.
  plot +
    ggplot2::coord_fixed(
      ratio  = 1,
      xlim   = lim,
      ylim   = lim,
      expand = object$expand
    )
}


#' Enforce Symmetry
#' 
#' Enforces symmetric plot design: y axis includes opposites of the range of the data.
#' @return ggplot_symmetric
#' @seealso ggplot_add.ggplot_symmetric
#' @export
#' @keywords internal
#' @family isometric
#' @param expand amount of scale expansion to allow, e.g. 0.05 (default 0 for best fidelity)
#' @examples
#' library(magrittr)
#' library(ggplot2)
#' data.frame(x = 1:10, y = c(-2, 5, 0, -1, 4, 0, 1, -3, 3, 0)) %>%
#' ggplot(aes(x, y)) + geom_point() + symmetric()
#' 
symmetric <- function(expand = c(top = FALSE, bottom = FALSE))structure(list(expand = expand), class = 'ggplot_symmetric')

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
  
  b  <- ggplot2::ggplot_build(plot)
  pp <- b$layout$panel_params
  
  get_y_range <- function(panel) {
    # ggplot2 version compatibility
    if (!is.null(panel[["y.range"]])) {
      panel[["y.range"]]
    } else if (!is.null(panel[["y"]]$range$range)) {
      panel[["y"]]$range$range
    } else {
      stop("Could not extract trained y-range from ggplot build.")
    }
  }
  
  y_ranges <- lapply(pp, get_y_range)
  
  # If facets have free y scales, panel ranges differ; base ggplot2
  # can't apply different coord limits per panel.
  same_y <- length(unique(vapply(y_ranges, paste, "", collapse = ","))) == 1
  if (!same_y && length(pp) > 1) {
    stop(
      "symmetric(): facets appear to use free y scales (panel ranges differ).\n",
      "Base ggplot2 can't enforce per-panel symmetric y-limits.\n",
      "Use fixed scales, or split+combine plots, or use an extension that supports per-facet scales."
    )
  }
  
  yr <- range(unlist(y_ranges), finite = TRUE)
  M  <- max(abs(yr), finite = TRUE)
  
  # Degenerate case: all y are 0/NA
  if (!is.finite(M) || M == 0) M <- 1
  
  plot + ggplot2::coord_cartesian(ylim = c(-M, M), expand = object$expand)
}

#' @export 
ggplot2::ggplot_add

