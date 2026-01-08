#' Substitute Expressions, Titles, Labels and Aesthetics in ggplots
#'
#' Substitutes expressions, titles, labels and aesthetics in decorated ggplots.
#' Default labels (mappings for \code{x}, \code{y},)
#' will be replaced (if possible)
#' the first available of corresponding data attributes in
#' \code{get_option('yamlet_decorated_ggplot_search', c('expression', 'title', 'label'))}.  
#' Likewise, if mappings for 
#' \code{getOption('yamlet_decorated_ggplot_discrete',c('colour', 'fill', 'size', 'shape', 'linetype', 'linewidth', 'alpha'))}
#' indicate columns that have these defined as attributes, 
#' an attempt is made to add a corresponding discrete scale if
#' one does not exist already. Values are recycled if necessary
#' and are specific by ordinal position to the corresponding
#' level of the corresponding variable.  Levels are defined
#' in increasing priority by
#' * \code{sort(unique(x))} (if \code{getOption('yamlet_decorated_ggplot_drop', TRUE)}),
#' * any guide attribute,
#' * any factor levels,
#' * any codelist attribute, or
#' * any plotmath attribute.
#'
#' @param plot class 'decorated_ggplot' from \code{\link{ggplot.decorated}}
#' @param ... ignored
#' @return see \code{\link[ggplot2]{ggplot_build}}
#' @export
#' @method ggplot_build decorated_ggplot
#' @importFrom ggplot2 ggplot_build waiver
#' @keywords internal
#' @family decorated_ggplot

ggplot_build.decorated_ggplot <- function(plot, ...){
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
    drop = drop,
    ...
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
    drop = getOption('yamlet_decorated_ggplot_drop', TRUE),
    ...
){
  # In 1.2.7 and later, we harmonize decorations across layers.
  # following ggplot convention, last definition trumps earlier

  decor <- as_decorated(data.frame()) # accumulator
  if(length(x$data)){
    decor <- bind_rows(slice(x$data, 0), decor) # trumps earlier
  }
  for(i in seq_along(x$layers)){
    if(length(x$layers[[i]]$data)){
      decor <- bind_rows(slice(x$layers[[i]]$data, 0), decor)
    }
  }
  
  # consensus decorations, with each layer trumping earlier.
  # enforce consensus
  if(length(x$data)){
    x$data <- redecorate(x$data, decor) # overwrite: true
  }
  for(i in seq_along(x$layers)){
    if(length(x$layers[[i]]$data)){
      x$layers[[i]]$data <- redecorate(x$layers[[i]]$data, decor) # overwrite: true
    }
  }
  
  context <- data_context(x)
  # theLabels <- get_labs(x)
  theLabels <- structure(context$map, names = context$aesthetic)
  aesthetics <- intersect(discrete, names(theLabels))
  scaletypes <- sapply(x$scales$scales, `[[`, 'aesthetics')
  # don't redefine existing scales:
  aesthetics <- setdiff(aesthetics, scaletypes)
  for(a in aesthetics){                 # color, fill, size, etc
    i <- context$data[context$aesthetic == a]
    # if i is zero, data is default
    # if greater than zero, data is in a layer
    data <- x$data
    if(i > 0) data <- x$layers[[i]]$data
    src <- theLabels[[a]]               # the corresponding label
    if(length(src) == 1){               # needs to be singular
      if(src %in% names(data)){       # and present in data
        col <- data[[src]]            # the column name
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
          levels(data[[src]]) <- plotmath  # implement in data
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
        
        # restore data
        if(i == 0) data -> x$data
        if(i > 0)  data -> x$layers[[i]]$data
        
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
  
  for(i in seq_along(theLabels)){           # x (gg object) stores names of used columns as $labels
    lab <- theLabels[[i]]                   # handle one label
    aesthetic <- names(theLabels)[[i]]
    j <- context$data[context$aesthetic == aesthetic]
    data <- x$data
    if(j > 0) data <- x$layers[[j]]$data
    if(length(lab)){                       # i.e. not null or empty expression
      if(length(lab) > 1){
        warning('length of labels for ', aesthetic, ' is longer than 1; using first value: ', lab[[1]] )
        lab <- lab[[1]]
      }
      if(lab %in% names(data)){            # if this is just a bare column name
        col <- data[[lab]]
        atr <- attributes(col)
        for( s in rev(search)){              # end with first
          label <- atr[[s]]                  # retrieve label
          if(!is.null(label)){
            # note well:
            # for 3.5.1 and before, x$label is being overwritten, as "theLabels" is essentially x$labels.
            # In later versions, x$labels represents manual user specifications and therefore
            # has been filtered from "theLabels" to *prevent* overwriting.
            # Therefore, this is a new assignment.
            if(length(label) > 1){ # expecting scalar labels for these aesthetics
              theFirst <- label[[1]]
              if(length(names(label)))label = paste(
                paste0(
                  '(',
                  names(label),
                  ')'
                ),
                label
              )
              label <- paste(label, collapse = '\n')
              msg <- paste('using first of', label, sep = '\n')
              warning(msg)
              # and finally, the substitution:
              label <- theFirst
            }
            x$labels[[aesthetic]] <- label  # assign default label with one from data attributes
          }
        }
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
  
  # in most cases, we are working with x$data
  # possibly, x$data is an empty list (unlikely for decorated_ggplot)
  # for consistency, we check data_context for first non-empty layer,
  # reading from and assigning to that.
  # data_context returns only layers with data.
  # 0 indicates default "layer".

  i <- context$layer[1] # NA if no rows (unlikely)
  if(is.na(i)){i <- 0}
  if(i == 0) data <- x$data
  if(i > 0)  data <- x$layers[[i]]$data
  
  hits <- 0L
  for(col in cols){
    if(!(col %in% names(data))) next
    this <- data[[col]]
    if(!inherits(this, 'factor')) next
    plotmath <- attr(this, 'plotmath')
    if(is.null(plotmath)) next
    levels(data[[col]]) <- plotmath
    hits <- hits + 1L
  }
  
  if(i == 0) data -> x$data
  if(i > 0)  data -> x$layers[[i]]$data
  
  
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

