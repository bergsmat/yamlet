#' Create a New ggplot for a Decorated Data Frame
#'
#' Creates a new ggplot object for a decorated data.frame.
#' This is the ggplot() method for class 'decorated'.
#' It creates a ggplot object using the default method,
#' but reclassifies it as 'decorated_ggplot' so that a custom print method
#' is invoked; see \code{\link{print.decorated_ggplot}}.
#'
#' This approach is similar to but more flexible than
#' the method for \code{\link{ggready}}.
# Currently,
# there is only one method for resolve() (\code{\link{resolve.decorated}})
# with the result that all 'resolved' objects inherit 'decorated'
# and thus can use \code{\link{ggplot.decorated}}.
#' For fine control, you can switch between 'data.frame'
#' and 'decorated' using \code{\link{as_decorated}}
#' (supplies null decorations) and \code{\link{as.data.frame}}
#' (preserves decorations).
#'
#' @param data decorated, see \code{\link{decorate}}
#' @param ... passed to \code{\link[ggplot2]{ggplot}}
#' @param search attribute names from which to seek label substitutes
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
#' map <- aes(x = time, y = conc, color = Heart)
#' data.frame(x) %>% ggplot(map) + geom_point()
#'
#' # Decorated data.frame uses supplied labels.
#' # Notice CHF levels are still not ordered. (Moderate first.)
#' x %>% ggplot(map) + geom_point()
#' 
#' # If we resolve Heart, CHF levels are ordered.
#' x %<>% resolve(Heart)
#' x %>% ggplot(map) + geom_point()
#' 
#' # We can map aesthetics as decorations.
#' x %<>% decorate('Heart: [ color: [gold, purple, green]]')
#' x %>% ggplot(map) + geom_point()
#' 
#' # Colors are matched to particular levels. Purple drops out here:
#' x %>% filter(Heart != 'Moderate') %>% ggplot(map) + geom_point()
#'
#' # We can resolve other columns for a chance to enrich the output with units.
#' x %<>% resolve
#' suppressWarnings( # because this complains for columns with no units
#'   x <- modify(x, title = paste0(label, '\n(', units, ')'))
#' )
#' x %>% ggplot(map) + geom_point()
#'
#' # Or something fancier.
#' x %<>% modify(conc, title = 'conc_serum. (mg*L^-1.)')
#' x %>% ggplot(map) + geom_point()
#'
#' # The y-axis title is deliberately given in spork syntax for elegant coercion:
#' library(spork)
#' x %<>% modify(conc, expression = as.expression(as_plotmath(as_spork(title))))
#' x %>% ggplot(map) + geom_point()

#' # Add a fancier label for Heart, and facet by a factor:
#' x %<>% modify(Heart, expression = as.expression(as_plotmath(as_spork('CHF^\\*'))))
#' x %>% ggplot(map) + geom_point() + facet_wrap(~Creatinine)
#'
#' # ggready handles the units and plotmath implicitly for a 'standard' display:
#' x %>% ggready %>% ggplot(map) + geom_point() + facet_wrap(~Creatinine)
#'
#' # Notice that instead of over-writing the label
#' # attribute, we are creating a stack of label
#' # substitutes (title, expression) so that
#' # label is still available as an argument
#' # if we want to try something else.  The
#' # print method by default looks for all of these.
#' # Precedence is expression, title, label, column name.
#' # Precedence can be controlled using
#' # options(decorated_ggplot_search = c(a, b, ...) ).
#'
#' # Here we try a dataset with conditional labels and units.
#'
#' file <- system.file(package = 'yamlet', 'extdata','phenobarb.csv')
#' x <- file %>% decorate %>% resolve
#' # Note that value has two elements for label and guide.
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
#' # print.decorated_ggplot will attempt to honor coordinated aesthetics.
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



ggplot.decorated <- function(
  data, 
  ...,
  search = getOption(
    'yamlet_decorated_ggplot_search',
    c('expression', 'title', 'label')
  )
){
    # newer versions of ggplot2 use label attribute as default label
    # old versions: see .decorated_ggplot
    if(gg_new()){ 
      for(s in rev(search)){
        # store old label
        suppressMessages(data %<>% modify(`_label` = label))
        # never any reason to assign 'label' to 'label'
        if(s == 'label') next
        suppressMessages(data %<>% modify(label = `$`(.data, s)))
      }
    }
  }
  p <- NextMethod()
  class(p) <- c('decorated_ggplot',class(p))
  p
}

#' Substitute Expressions, Titles, Labels and Aesthetics in ggplots
#'
#' Default labels (e.g. mappings for \code{x}, \code{y}, etc.)
#' will be used to search \code{data} for more meaningful
#' labels, taking first available from attributes
#' with names in \code{search}.  Likewise, if mappings for 
#' colour (color), fill, size, etc. (see defaults for \code{discrete})
#' indicate columns that have these defined as attributes, 
#' an attempt is made to add a corresponding discrete scale if
#' one does not exist already. Values are recycled if necessary
#' and are specific by ordinal position to the corresponding
#' level of the corresponding variable.  Levels are defined
#' in increasing priority by
#' \code{sort(unique(x))},
#' any guide attribute,
#' any factor levels,
#' any codelist attribute, or
#' any plotmath attribute.
#' 
#' 
#'
#' @param x class 'decorated_ggplot' from \code{\link{ggplot.decorated}}
#' @param ... ignored
#' @param search attribute names from which to seek label substitutes
#' @param discrete discrete aesthetics to map from data decorations where available
#' @param drop should unused factor levels be omitted from data-driven discrete scales?
#' @return see \code{\link[ggplot2]{print.ggplot}}
#' @importFrom ggplot2 scale_discrete_manual waiver
#' @export
#' @family decorated_ggplot
#' @examples
#' example(ggplot.decorated)

print.decorated_ggplot <- function(
    x,
    ...,
    search = getOption(
      'yamlet_decorated_ggplot_search',
      c('expression', 'title', 'label')
    ),
    discrete = getOption(
      'yamlet_decorated_ggplot_discrete',
      c('colour', 'fill', 'size', 'shape', 'linetype', 'linewidth', 'alpha')
    ),
    drop = getOption('yamlet_decorated_ggplot_drop', TRUE)
){
  x <- .decorated_ggplot(
    x = x,
    search = search,
    discrete = discrete,
    drop = drop
  )
  NextMethod()
}

.decorated_ggplot <- function(
    x,
    search = getOption(
      'yamlet_decorated_ggplot_search',
      c('expression', 'title', 'label')
    ),
    discrete = getOption(
      'yamlet_decorated_ggplot_discrete',
      c('colour', 'fill', 'size', 'shape', 'linetype', 'linewidth', 'alpha')
    ),
    drop = getOption('yamlet_decorated_ggplot_drop', TRUE)
){

  # support for discrete manual scales
  theLabels <- x$labels
  if(gg_new()){
    theLabels <- get_labs(x)
    # ignore labels that were explicitly set
    candidates <- names(theLabels)
    userdefined <- x$labels
    keep <- setdiff(candidates, userdefined)
    theLabels <- theLabels[keep]
  }
  labelnames <- names(theLabels)
  aesthetics <- intersect(discrete, labelnames)
  scaletypes <- sapply(x$scales$scales, `[[`, 'aesthetics')
  # don't redefine existing scales:
  aesthetics <- setdiff(aesthetics, scaletypes)
  for(a in aesthetics){                 # color, fill, size, etc
    src <- theLabels[[a]]               # the corresponding label
    if(length(src) == 1){               # needs to be singular
      if(src %in% names(x$data)){       # and present in data
        col <- x$data[[src]]            # the column name
        atr <- attributes(col)
        nms <- names(atr)
        if('color' %in% nms & !'colour' %in% nms){
          atr$colour <- atr$color
        }
        # now we want to make one new scale
        # for this aesthetic
        # if the column has a matching attribute
        # or if the column is parseable.
        need_scale <- (a %in% names(atr)) | ('plotmath' %in% names(atr))
        if(!need_scale) next
        
        # calculate values
        this <- atr[[a]]
        # preserve correspondence with guides
        # increasing precedence:
        levels <- sort(unique(col))
        if('guide' %in% names(atr)) levels <- atr$guide
        if(is.factor(col)) levels <- levels(col)
        if('codelist' %in% names(atr)) levels <- atr$codelist # ignore names
        
        # support plotmath
        if('plotmath' %in% names(atr) & inherits(col, 'factor')){
          plotmath      <- attr(col, 'plotmath')
          levels(x$data[[src]]) <- plotmath  # implement in data
          levels(col)           <- plotmath  # implement locally
          levels <- plotmath                 # implement here
        }
        if(!is.null(this)){this <- rep(this, length.out = length(levels))}
        if(!is.null(this)){names(this) <- levels}
        this <- unlist(this)
        
        # calculate breaks
        breaks <- waiver()
        if(drop) breaks <- sort(unique(col))
        
        # create a new scale using the stored values
        # scale_discrete_manual('color'...) must have values and aesthetics
        # scale_color_discrete must not have values nor aesthetics
        # both accept labels and breaks
        args <- list(breaks = breaks)
        if(!is.null(this)){
          args <- list(aesthetics = a, values = this, breaks = breaks)
        }
        if('plotmath' %in% names(atr)){
          args <- c(args, list(labels = scales::label_parse()))
        }
        
        # arg list complete
        # calculate function name
        fun <- match.fun('scale_discrete_manual')
        if(is.null(this)){
          fun <- match.fun(paste(sep = '_', 'scale', a, 'discrete'))
        }
        theScale <- do.call(fun, args)
        x <- x + theScale
      }
    }
  }
  if(!gg_new()){                              # for gg_new(), see ggplot.decorated
  for(i in seq_along(x$labels)){           # x (gg object) stores names of used columns as $labels
    lab <- x$labels[[i]]                   # handle one label
    if(length(lab)){                       # i.e. not null or empty expression
      if(length(lab) == 1){
        if(lab %in% names(x$data)){            # if this is just a bare column name
          col <- x$data[[lab]]
          atr <- attributes(col)
          for( s in rev(search)){              # end with first
            label <- atr[[s]]                  # retrieve label
            if(!is.null(label)){
              x$labels[[i]] <- label           # overwrite default label with one from data attributes
            }
          }
        } 
      }
      # done with search.  Plural labels? Note x$labels unchanged, lab unchanged
      if(length(lab) > 1){
        if(length(names(lab)))lab = paste(
          paste0(
            '(',
            names(lab),
            ')'
          ),
          lab
        )
        lab <- paste(lab, collapse = '\n')
        msg <- paste('using first of', lab, sep = '\n')
        warning(msg)
        x$labels[[i]] <- x$labels[[i]][[1]]
      }
    }
  }
  
  # above, support for aesthetics has the side effect
  # of transforming the underlying data where 
  # plotmath versions of factor levels are available.
  # scales are created with matching levels.
  # scales::label_parse() does the markup.
  
  # But when variables are used in facets,
  # their names show up not in x$labels
  # but in names(x$facet$params$facets) (facet_wrap())
  # or  in names(x$facet$params$rows)
  # or  in names(x$facet$params$cols).
  # we need to check whether these are cols in $data
  # and whether they have plotmath attributes
  # and are actually factors.
  # If so, we assign plotmath to levels(col), possibly redundant.
  # Additionally, if x$facet$params$labeller is just ggplot2::label_value (default)
  # we assign ggplot2::label_parsed().
  
  cols <- c(
    names(x$facet$params$facets),
    names(x$facet$params$rows),
    names(x$facet$params$cols)
  )
  
  hits <- 0L
  for(col in cols){
    if(!(col %in% names(x$data))) next
    this <- x$data[[col]]
    if(!inherits(this, 'factor')) next
    plotmath <- attr(this, 'plotmath')
    if(is.null(plotmath)) next
    levels(x$data[[col]]) <- plotmath
    hits <- hits + 1L
  }
  if(hits){
    if(
      identical( # default is unperturbed
        x$facet$params$labeller,
        ggplot2::label_value
      )
    ){
      # treat levels as expressions
      x$facet$params$labeller <- ggplot2::label_parsed
    }
  }
  return(x)
}

#' Detect Revised Label Strategy
#' 
#' Detects the existence of qqplot's updated label strategy after v. 3.5.1,
#' e.g. ggplot2_3.5.1.9000. For internal use to accommodate breaking changes.
#' 
gg_new <- function()"get_labs" %in% getNamespaceExports("ggplot2")

#' Get Labels
#' 
#' Gets labels for a ggplot object.  Not exported, to avoid confusion.
#' Development version of ggplot2 implements new get_labs() and get_strip_labels()
#' interfaces. This function is an abstraction that supports new vs old approaches.
#' See https://github.com/tidyverse/ggplot2/pull/6078.
#' 
#' @param plot the ggplot
#' 
get_labs <- if (gg_new()) {
  ggplot2::get_labs
} else {
  function(plot) plot$labels
}

