#' Enable Automatic Labels and Units for ggplot
#'
#' Enable automatic labels and units for ggplot.
#' Substitutes column label, if present, for default.
#' Supports arrangements of ggplot objects.
#' Defined similarly to \code{\link{print.decorated_ggplot}}
#' and respects global options 
#' yamlet_decorated_ggplot_search,
#' yamlet_decorated_ggplot_discrete, and
#' yamlet_decorated_ggplot_drop.
#'
#' @param plot class 'decorated_ggplot' from \code{\link{ggplot.decorated}}
#' @return see \code{\link[ggplot2]{ggplot_build}}
#' @export
#' @method ggplot_build decorated_ggplot
#' @importFrom ggplot2 ggplot_build
#' @keywords internal
#' @family decorated_ggplot

ggplot_build.decorated_ggplot <- function(plot){
  search = getOption(
    'yamlet_decorated_ggplot_search',
    c('expression', 'title', 'label')
  )
  discrete = getOption(
    'yamlet_decorated_ggplot_discrete',
    c('colour', 'fill', 'size', 'shape', 'linetype', 'linewidth', 'alpha')
  )
  drop = getOption('yamlet_decorated_ggplot_drop', TRUE)
  
  plot <- .decorated_ggplot(
    x = plot,
    search = search,
    discrete = discrete,
    drop = drop
  )

  built <- NextMethod()
  return(built)
}
