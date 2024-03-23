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
  
x <- plot
  # # support for plotmath levels
  # parseable <- character(0)
  # for(col in x$labels){
  #   plotmath <- attr(x$data[[col]], 'plotmath')
  #   if(is.null())
  #   if(!is.null(plotmath)){
  #     parseable <- c(parseable, col) # accumulate to inform scales below
  #     # number of levels should exactly match length of plotmath
  #     levels(x$data[[col]]) <- plotmath
  #   }
  # }
  
  # support for discrete manual scales
  labelnames <- names(x$labels)
  aesthetics <- intersect(discrete, labelnames)
  scaletypes <- sapply(x$scales$scales, `[[`, 'aesthetics')
  # don't redefine existing scales:
  aesthetics <- setdiff(aesthetics, scaletypes)
  for(a in aesthetics){                 # color, fill, size, etc
    src <- x$labels[[a]]                # the corresponding label
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
        this <- rep(this, length.out = length(levels))
        if(!is.null(this)){this <- rep(this, length.out = length(levels))}
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
  plot <- x
  NextMethod()
}
