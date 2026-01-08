globalVariables(c('x','y'))
#' Create a New ggplot for a Decorated Data Frame
#'
#' Creates a new ggplot object for a decorated data.frame.
#' This is the ggplot() method for class 'decorated'.
#' It creates a ggplot object using the default method,
#' but reclassifies it as 'decorated_ggplot' so that a custom build method
#' is invoked; see \code{\link{ggplot_build.decorated_ggplot}}.
#'
# This approach is similar to but more flexible than
# the method for \code{\link{ggready}}.
# Currently,
# there is only one method for resolve() (\code{\link{resolve.decorated}})
# with the result that all 'resolved' objects inherit 'decorated'
# and thus can use \code{\link{ggplot.decorated}}.

#' For fine control, you can switch between 'data.frame'
#' and 'decorated' using \code{\link{as_decorated}}
#' (supplies null decorations) and \code{\link{as.data.frame}}
#' (preserves decorations).
#' 
# As of version 1.2.7, an attempt is made to support
# decorations in successive layers. If more than 
# one dataset is involved, decorations accumulate in
# layer order (beginning with the default data, if any)
# and are enforced on successive layers. For finer
# control, limit "competing" datasets to those variables
# used in the layer.
# As of version 1.3.0, an attempt is made to seek
# decorations for a layer in its own data.
# precedence for each aesthetic:
# 1. decorations from the data for the first layer that defines the aesthetic and provides corresponding data.
# 2. decorations from the default data
#'
#' @param data decorated, see \code{\link{decorate}}
#' @param ... passed to \code{\link[ggplot2]{ggplot}}
#' @return return value like \code{\link[ggplot2]{ggplot}} but inheriting 'decorated_ggplot'
#' @export
#' @importFrom ggplot2 ggplot
#' @family decorated_ggplot
#' @family interface
#' @seealso decorate resolve ggready
#' @examples
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' library(ggplot2)
#' library(dplyr)
#' library(magrittr)
#' # par(ask = FALSE)
#'
#' x <- decorate(file)
#' x %<>% filter(!is.na(conc))
#'
#' # Manipulate class to switch among ggplot methods.
#' class(x)
#' class(data.frame(x))
#' class(as_decorated(data.frame(x)))
#'
#' # The bare data.frame gives boring labels and un-ordered groups.
#' # (After ggplot2 v. 3.5.1 label attributes are honored as axis labels.)
#' map <- aes(x = time, y = conc, color = Heart)
#' data.frame(x) %>% ggplot(map) + geom_point()
#'
#' # Decorated data.frame uses supplied labels.
#' # Notice CHF levels are still not ordered. (Moderate first.)
#' x %>% ggplot(map) + geom_point()
#' 
#' # If we resolve Heart, CHF levels are ordered.
#' x %>% resolve(Heart) %>% ggplot(map) + geom_point()
#' 
#' # We can map aesthetics as decorations.
#' x %>% 
#'   decorate('Heart: [ color: [gold, purple, green]]') %>%
#'   ggplot(map) + geom_point()
#' 
#' # Colors are matched to particular levels. Purple drops out here:
#' x %>% 
#'   decorate('Heart: [ color: [gold, purple, green]]') %>%
#'   filter(Heart != 'Moderate') %>%
#'   ggplot(map) + geom_point()
#'
#' # We can resolve other columns for a chance to enrich the output with units.
#' x %>%
#'   resolve %>%
#'   ggplot(map) + geom_point()
#'
#' # Underscore and circumflex imply subscript and superscript:
#' x %>% 
#'   redecorate("conc: [ conc_serum, mg*L^-1 ]") %>%
#'   ggplot(map) + geom_point()
#'
#' # If we invoke enscript(), the subscripts and superscripts are rendered: 
#' x %>% 
#'   redecorate("conc: [ conc_serum, mg*L^-1 ]") %>%
#'   redecorate("Heart: [ CHF^\\* ]") %>%
#'   enscript %>%
#'   ggplot(map) + geom_point()
#'
#' # Here we try a dataset with conditional labels and units.
#'
#' file <- system.file(package = 'yamlet', 'extdata','phenobarb.csv')
#' x <- file %>% decorate %>% resolve
#' # Note that value has two elements for label, etc.
#' x %>% decorations(value)
#'
#' # The print method defaults to the first, with warning.
#' map <- aes(x = time, y = value, color = event)
#' \donttest{
#' x %>% ggplot(map) + geom_point()
#' }
#'
#' # If we subset appropriately, the relevant value is substituted.
#' x %>% filter(event == 'conc') %>% ggplot(map) + geom_point()
#'
#' x %>% filter(event == 'conc') %>%
#' ggplot(aes(x = time, y = value, color = ApgarInd)) + geom_point()
#'
#' x %>% filter(event == 'dose') %>%
#' ggplot(aes(x = time, y = value, color = Wt)) +
#' geom_point() +
#' scale_y_log10() +
#' scale_color_gradientn(colours = rainbow(4))
#' 
#' # ggplot_build.decorated_ggplot will attempt to honor coordinated aesthetics.
#' x <- data.frame(x = c(1:6, 3:8), y = c(1:6,1:6), z = letters[c(1:6,1:6)])
#' x %<>% decorate('z: [color: ["red", "blue", "green", "gold", "black", "magenta"]]')
#' x %<>% decorate('z: [fill: ["red", "blue", "green", "gold", "black", "magenta"]]')
#' x %<>% decorate('z: [shape: [20, 21, 22, 23, 24, 25]]')
#' x %<>% decorate('z: [linetype: [6, 5, 4, 3, 2, 1]]')
#' x %<>% decorate('z: [alpha: [ .9, .8, .7, .6, .5, .4]]')
#' x %<>% decorate('z: [size: [1, 1.5, 2, 2.5, 3, 3.5]]')
#' x %>% ggplot(aes(
#'  x, y,
#'   color = z,
#'   fill = z,
#'   shape = z,
#'   linetype = z, 
#'   alpha = z,
#'   size = z,
#' )) + 
#'   geom_point() +
#'   geom_line(size = 1)



ggplot.decorated <- function(data, ...){
  p <- NextMethod()
  class(p) <- c('decorated_ggplot',class(p))
  p
}

#  Substitute Expressions, Titles, Labels and Aesthetics in ggplots
# 
#  Default labels (e.g. mappings for \code{x}, \code{y}, etc.)
#  will be used to search \code{data} for more meaningful
#  labels, taking first available from attributes
#  with names in \code{search}.  Likewise, if mappings for 
#  colour (color), fill, size, etc. (see defaults for \code{discrete})
#  indicate columns that have these defined as attributes, 
#  an attempt is made to add a corresponding discrete scale if
#  one does not exist already. Values are recycled if necessary
#  and are specific by ordinal position to the corresponding
#  level of the corresponding variable.  Levels are defined
#  in increasing priority by
#  \code{sort(unique(x))},
#  any guide attribute,
#  any factor levels,
#  any codelist attribute, or
#  any plotmath attribute.
#  
#  
# 
#  @param x class 'decorated_ggplot' from \code{\link{ggplot.decorated}}
#  @param ... ignored
#  @param search attribute names from which to seek label substitutes
#  @param discrete discrete aesthetics to map from data decorations where available
#  @param drop should unused factor levels be omitted from data-driven discrete scales?
#  @return see \code{\link[ggplot2]{print.ggplot}}
#  @importFrom ggplot2 scale_discrete_manual waiver
#  @importFrom dplyr bind_rows
#  @export
#  @family decorated_ggplot
#  @examples
#  example(ggplot.decorated)

# print.decorated_ggplot <- function(
#     x,
#     ...,
#     search = getOption(
#       'yamlet_decorated_ggplot_search',
#       c('expression', 'title', 'label')
#     ),
#     discrete = getOption(
#       'yamlet_decorated_ggplot_discrete',
#       c('colour', 'fill', 'size', 'shape', 'linetype', 'linewidth', 'alpha')
#     ),
#     drop = getOption('yamlet_decorated_ggplot_drop', TRUE)
# ){
#   # x <- .decorated_ggplot(
#   #   x = x,
#   #   search = search,
#   #   discrete = discrete,
#   #   drop = drop
#   # )
#   NextMethod()
# }


#' Detect Revised Label Strategy
#' 
#' Detects the existence of qqplot's updated label strategy after v. 3.5.1,
#' e.g. ggplot2_3.5.1.9000. For internal use to accommodate breaking changes.
#' 
# this ultimately failed due to altered release strategy
# gg_new <- function()"get_labs" %in% getNamespaceExports("ggplot2")
# this is more empirical.
gg_new <- function(){
  p <- ggplot2::ggplot(data.frame(x=1, y=1), aes(x,y))
  labs <- p$labels
  # if this is old-school, labs is a list with members x and y.  
  # if this is the "new" way, labs is an empty list, to be calculated on build.
  is.new <- length(labs) == 0
  return(is.new)
}

#' Get Labels
#' 
#' Gets labels for a ggplot object.  Not exported, to avoid confusion.
#' Development version of ggplot2 implements new get_labs() interface.
#' This function is an abstraction that supports new vs old approaches,
#' solely for yamlet's interests.
#' See https://github.com/tidyverse/ggplot2/pull/6078.
#' 
#' @param plot the ggplot
#' 
get_labs <- function(plot) {
  theLabels <- plot$labels # old style default
  
  if (gg_new()) { # re-define theLabels
    
    # ggplot_build.decorated_ggplot calls get_labs
    # as do the ggplot_symmetric and ggplot_isometric methods # no longer true as of 1.2.9
    # for ggplot_add()
    
    # get_labs builds the plot to assess the labels.
    # we need to know the old-style labels so that
    # we can locate value-added metadata by column.
    # thus we need to suppress label attributes here
    # to defeat their new-style uptake.

    # Specifically, ggplot2 plans/intends that
    # the label attributes of columns named as aesthetics
    # will be returned by get_labels() even if not assigned
    # in plot$labels, e.g. by labs(x=,) etc.
    # this makes get_labs() less-informative,
    # because the default mapping info is lost.
    # (available elsewhere?)
    # I.e., we no longer know what column in the dataset
    # is being used for that aesthetic (e.g., x)
    # so we can't go back and find other clever attributes
    # such as units, colors, and what not.
    # If we yeet the labels, get_labels() gives us
    # boring but useful mappings with which to work.
    # we practice on a decoy so that we don't really
    # make 'plot' less informative.
    
    decoy <- plot
    if(length(decoy$data)){ # maybe no default data
      decoy$data <- modify(decoy$data, label = NULL)
    }
    
    # TTB 12/18/2025 do this also for each layer
    for(i in seq_along(decoy$layers)){
      if(length(decoy$layers[[i]]$data)){ # maybe NULL, maybe class 'waiver': list()
        decoy$layers[[i]]$data <-  modify(decoy$layers[[i]]$data, label = NULL)
      }
    }
    
    # partial unclass to avoid circularity ...
    class(decoy) <- setdiff(class(decoy), 'decorated_ggplot')
    
    theLabels <- ggplot2::get_labs(decoy) # re-defined
    
    # ignore labels that were explicitly set
    candidates <- names(theLabels)
    userdefined <- names(plot$labels)
    keep <- setdiff(candidates, userdefined)
    
    # note, however, that warnings based on $labels
    # will not necessarily work across all ggplot2 versions.
    # consider above: warning('length of labels for ', aesthetic, ' is longer than 1
    # in the old days, reading plot$labels,
    # we would trap a multi-line title 
    # and only use the first element, with warning.
    # but now, (new way), something defined by
    # e.g. ggtitle() is considered an inviolable user intervention
    # (see userdefined above) 
    # and is therefore unmolested (and un-checked).
    # fortunately, ggplot silently uses only the first
    # line of a multiline label,
    # so we will quietly allow that under the new paradigm.
    
    theLabels <- theLabels[keep]
  }
  
  # unconditionally ...
  
  return(theLabels)
}
