#' Display Global Yamlet Options
#' 
#' Displays global yamlet options: those options
#' whose names begin with 'yamlet_'.
#' * **yamlet_append_units_open**: see \code{\link{append_units.default}}.
#'   Controls how labels are constructed for variables
#'   with 'units' attributes.  In brief, units are wrapped in parentheses, 
#'   and appended to the label.
#' * **yamlet_append_units_close**: see \code{\link{append_units.default}}.
#'   Controls how labels are constructed for variables
#'   with 'units' attributes.  In brief, units are wrapped in parentheses, 
#'   and appended to the label.
#' * **yamlet_append_units_style**: see \code{\link{append_units.default}}.
#'   Determines parsing as 'plotmath' or 'latex', or 'plain' for no parsing. 
#' * **yamlet_append_units_target**: see \code{\link{append_units.default}}.
#'   By default, append result is assigned to attribute 'label', but could be
#'   something else like 'title'.
#' * **yamlet_default_keys**: see \code{\link{as_yamlet.character}}.
#'   The first two yaml attributes without specified names
#'   are assumed to be 'label' and 'guide'.
#' * **yamlet_persistence**: see \code{\link{decorate.list}} and 
#'   \code{\link{as.integer.classified}}. By default, persistence
#'   of column attributes is implemented by creating 'dvec' objects
#'   (decorated vectors) using \pkg{vctrs} methodology.
#' * **yamlet_cell_value**: see \code{\link{as.data.frame.yamlet}}. 
#'   Controls how cells are calculated when converting yamlet
#'   (decorations) to a data.frame.
#' * **yamlet_import**: see \code{\link{decorate.character}}.
#'   Controls how primary data is read from file (default: as.csv()).
#' * **yamlet_extension**: see \code{\link{decorate.character}}.
#'   Controls what file extension is expected for yaml metadata
#'   (default: '.yaml')
#' * **yamlet_overwrite**: see \code{\link{decorate.list}}.
#'   Controls whether existing decorations are overwritten.
#' * **yamlet_exclude_attr**: see \code{\link{decorations.data.frame}}
#'   Controls what attributes are excluded from display.
#' * **yamlet_with_title**: see \code{\link{make_title.dvec}} and \code{\link{drop_title.dvec}}.
#'   For objects with (implied) units attributes, titles are by default
#'   automatically created on resolve() and destroyed on desolve().
#'   Interacts with yamlet_append_units_*.
#' * **yamlet_infer_guide**: see \code{\link{explicit_guide.yamlet}}.
#'   Identifies the function that will be used to reclassify 'guide' as something
#'   more explicit.
#' * **yamlet_explicit_guide_overwrite**: see \code{\link{explicit_guide.data.frame}}
#'   and \code{\link{explicit_guide.dvec}}. In the latter case, controls
#'   whether existing attributes are overwritten.
#' * **yamlet_explicit_guide_simplify**: \code{\link{explicit_guide.data.frame}}
#'   and \code{\link{explicit_guide.dvec}}. Ordinarily, the 'guide' attribute
#'   is removed if something more useful can be inferred.
#' * **yamlet_decorated_ggplot_search**: see \code{\link{print.decorated_ggplot}}.
#'   The print method for decorated_ggplot populates axis labels by searching
#'   first for attributes named 'expression', 'title', and 'label'.  Customizable.
#' * **yamlet_decorated_ggplot_discrete**: see \code{\link{print.decorated_ggplot}}.
#'   Discrete aesthetics to map from data decorations where available.
#' * **yamlet_decorated_ggplot_drop**: see \code{\link{print.decorated_ggplot}}.
#'   Should unused factor levels be omitted from data-driven discrete scales?
#' * **yamlet_ggready_parse**: see \code{\link{ggready.data.frame}}, 
#'   \code{\link{ggready.decorated}}. Whether to parse axis labels.
#'   TRUE by default, but may be problematic if unintended.
#' * **yamlet_modify_reserved**: see \code{\link{modify.default}}. A list of
#'   reserved labels that warn on reassignment.
#' * **yamlet_promote_reserved**: see \code{\link{promote.list}}.  
#'   Attributes to leave untouched when promoting singularities.
#' * **yamlet_promote**: see \code{\link{filter.decorated}}.
#'   Whether to promote when filtering 'decorated'.
#' 
#' @export
#' @md
#' @return list
#' @examples 
#' yamlet_options()
yamlet_options <- function(){
  opts <- options()
  nms <- names(opts)
  nms <- nms[grepl('^yamlet_',nms)]
  opts <- opts[nms]
  opts
}
