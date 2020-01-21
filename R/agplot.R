#' Coerce to Axis Label
#'
#' Converts to axis label. Generic, with method \code{\link{as_lab.list}}.
#' @param x object
#' @param ... passed arguments
#' @return see methods; typically length-one character
#' @export
#' @family lab
as_lab <- function(x,...)UseMethod('as_lab')

#' Coerce List to Axis Label
#'
#' Coerces list to axis label. Accepts a default label
#' and returns that if nothing better can be done.
#' If the attribute list has one named 'label', it
#' is chosen as a substitute.  But if that attribute
#' is itself a list of values, an attempt is made
#' to identify a single relevant value by treating
#' the value names as conditions to evaluate on
#' the supplied data. If a suitable value is
#' found, it is chosen as a substitute.  See
#' \code{\link{singularity}} for search logic.
#'
#' @param x list, such as returned by \code{\link{attributes}}.
#' @param default a value to return by default
#' @param collapse character: separator for collapsing multi-line units
#' @param enclose length-two character for enclosing unit
#' @param data data.frame for resolving competing named values
#' @param ... ignored
#' @return length-one character
#' @export
#' @keywords internal
#' @family lab
#' @examples
#' meta <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' x <- decorate(meta)
#' as_lab(attributes(x$time), 'time', enclose = c('[',']'))
#' as_lab(attributes(x$time), 'time', enclose = c('[ ',' ]'))
as_lab.list <- function(
  x,
  default,
  collapse = '\n',
  enclose = getOption('yamlet_enclose', default = c('(',')')),
  data,
  ...
){
  stopifnot(length(default) == 1, is.character(default))
  stopifnot(length(enclose) == 2, is.character(enclose))
  out <- default
  if('label' %in% names(x)){
    candidate <- x$label
    if(length(candidate) == 1){
      out <- unlist(candidate)
    }else{
      # multiple labels
      dex <- singularity(
        names(candidate),
        data
      )
      if(!is.na(dex)){
        if(dex > 0){
          out <- candidate[[dex]]
        }
      }
    }
  }
  more <- character(0)
  if('units' %in% names(x)) more <- x$units
  if('unit' %in% names(x)) more <- x$unit
  if('guide' %in% names(x)) more <- x$guide # such as encoding or unit
  # if(length(x$guide) == 1)
  #if(length(more) > 1) more <- paste(more, collapse = collapse)
  if(length(more) > 1){ # named levels? or conditional units?
    dex <- singularity(names(more), data)
    if(!is.na(dex)){
      if(dex > 0){
         more <- more[[dex]]
      }else{
        more <- character(0)
      }
    }else{
      more <- character(0)
    }
  }
  if(length(more)) {
    # at this point, more should be length-one character
    # just in case, we can collapse it to ensure singularity
    if(length(more) > 1){
      more <- paste(more, collapse = collapse)
    }
    more <- paste0(enclose[[1]], more, enclose[[2]])
    out <- paste(out, more)
  }
  out
}


#' Choose Singular Expression
#'
#' For a list of expressions evaluated on a data.frame
#' this returns the index of the one expression that evaluates
#' to an all-true vector (after coercing NA to FALSE).
#' Returns 0 if no expressions succeed, and NA_integer_ if
#' more than one succeed.
#'
#' @param x list of expressions
#' @param data data.frame
#' @param ... ignored
#' @export
#' @keywords internal
#' @return integer, possibly NA
#' @family lab
#' @examples
#' meta <- system.file(package = 'yamlet', 'extdata','phenobarb.csv')
#' x <- read.csv(meta)
#' singularity(
#'   data = x,
#'   list(
#'     "event == 'conc'",
#'     "event == 'dose'",
#'     "event == 'metabolite'"
#'   )
#' )
#' singularity(
#'   data = x[x$event == 'dose',],
#'   list(
#'     "event == 'conc'",
#'     "event == 'dose'",
#'     "event == 'metabolite'"
#'   )
#' )
#' singularity(
#'   data = x[x$event == 'dose',],
#'   list(
#'     "time >= 0",
#'     "event == 'dose'"
#'   )
#' )
#
singularity <- function(x, data, ...){
  if(!length(x))return(0)
  exprs <- lapply(x, function(i)parse(text = i))
  vals <- lapply(exprs, function(i)try(eval(i, envir = data, enclos = NULL)))
  defined <- lapply(vals, function(i){
    if(inherits(i, 'try-error')) i <- FALSE
    i <- as.logical(i)
    i[is.na(i)] <- FALSE
    i
  })
  condensed <- sapply(defined, all)
  res <- sum(condensed)
  if(res == 0) return(as.integer(res))
  if(res > 1) return(NA_integer_)
  # res = 1
  res <- seq_along(condensed)[condensed]
  stopifnot(length(res) == 1)
  res
}


#' Create a New ggplot for a Decorated Data Frame
#'
#' Creates a new ggplot object for a decorated data.frame.
#' This is the ggplot() method for class 'decorated';
#' it tries to implement automatic labels and units in axes and legends
#' in association with \code{\link{print.ag}}.
#' Use \code{ggplot(as.data.frame(x))} to get default
#' ggplot() behavior.
#'
#' @param data data.frame or similar
#' @param ... passed to \code{\link[ggplot2]{ggplot}}
#' @return return value like \code{\link[ggplot2]{ggplot}}
#' @export
#' @importFrom ggplot2 ggplot
#' @family lab
#' @examples
#' meta <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' x <- decorate(meta)
#' library(ggplot2)
#' class(ggplot(data = x) + geom_path(aes(x = time, y = conc)))
#' class(ggplot(data = x, aes(x = time, y = conc)) + geom_path())
#' example(print.ag)

ggplot.decorated <- function(data, ...){
  class(data) <- setdiff(class(data), 'decorated')
  p <- ggplot(data = data, ...)
  class(p) <- c('ag',class(p))
  p
}
#' Print Automatic Labels and Units for ggplot
#'
#' Prints automatic labels and units for ggplot.
#' Reworks the labels as a function of attributes
#' in corresponding data. Default for \code{labeller}
#' (\code{\link{as_lab}}) will
#' receive existing labels one at a time
#' and corresponding attributes (if any) from data.
#'
#' @param x class 'ag' from \code{\link{ggplot.decorated}}
#' @param labeller a function (or its name) like \code{\link{as_lab}} to generate axis labels
#' @param ... passed arguments
#' @return used for side effects
#' @export
#' @family lab
#' @examples
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' library(ggplot2)
#' library(dplyr)
#' library(magrittr)
#' # par(ask = FALSE)
#' options(enclose = c('[ ',' ]'))
#'
#' # Filter() strips 'label' from factors (see legend), but not vectors:
#'
#' file %>% decorate %>% resolve %>% filter(!is.na(conc)) %>%
#' ggplot(aes(x = time, y = conc, color = Heart)) + geom_point()
#'
#' # No factors created here, but print.ag promotes to factor if it can:
#'
#' file %>% decorate %>% filter(!is.na(conc)) %>%
#' ggplot(aes(x = time, y = conc, color = Heart)) + geom_point()
#'
#' # Here we try a dataset with conditional labels and units.
#'
#' file <- system.file(package = 'yamlet', 'extdata','phenobarb.csv')
#'
#' # Note that there are two elements each for value label and value guide.
#' #'
#' file %>% decorate %>% as_yamlet

#' # Guide might have been mistaken for an attempt to provide codes/decodes
#' # for a factor.  However, the keys evaluate to logical on the data.frame.
#' # Seeing that, we test for one of them being all true, and if so we select it.
#'
#' file %>% decorate %>% ggplot(aes(x = time, y = value, color = event)) + geom_point()
#'
#' # In the above example, we are plotting doses and concentrations, which have
#' # different labels and units, so we can't improve on the y axis label.
#' # But if we subset to just one of these, then only one of the named conditions
#' # will be always true (and will therefore be promoted).
#'
#' file %>% decorate %>%
#' filter(event == 'conc') %>%
#' ggplot(aes(x = time, y = value, color = ApgarInd)) + geom_point()
#'
#' file %>% decorate %>%
#' filter(event == 'dose') %>%
#' ggplot(aes(x = time, y = value, color = Wt)) +
#' geom_point() +
#' scale_y_log10() +
#' scale_color_gradientn(colours = rainbow(4))
#'
# file %>% decorate %>%
# ggplot(aes(x = time, y = value, color = event)) +
# geom_point() +
# facet_wrap(~ event, scales = 'free_y')


print.ag <- function(x, labeller = getOption('yamlet_labeller', default = as_lab), ...){
  fun <- match.fun(labeller)
  for(i in seq_along(x$labels)){           # x (gg object) stores names of used columns as $labels
    lab <- x$labels[[i]]                   # deal with one label

    if(lab %in% names(x$data)){            # if this is just a bare column name
      attr <- attributes(x$data[[lab]])    # retrieve the attributes
      if(!is.null(attr)){
        val <- fun(x = attr, default = lab, data = x$data, ...)
        x$labels[[i]] <- val               # replace default label with one from labeller
      }
      # while we are here, we should
      #promote lab to factor if appropriate
      guide <- attr$guide
      table <- x$data[[lab]]
      if(length(guide) > 1){
        if(!isConditional(guide, x$data)){
          if(!is.factor(table)){ # is.vector returns false if x has non-name attributes
            if(isLevels(guide, table)){
              labels <- as.character(guide)
              if(length(names(guide))){
                if(!any(names(guide) == '')){
                  labels <- names(guide)
                }
              }
              try(
                x$data[[lab]] <- factor(
                  x$data[[lab]],
                  levels = as.character(attr$guide),
                  labels = labels
                )
              )
            }
          }
        }
      }
    }
  }
  NextMethod()
}

#' Test Expression is Conditional
#'
#' Tests whether expression is conditional.
#' @param x character
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family conditional
#' @return logical
isConditional <- function(x, ...)UseMethod('isConditional')

#' Test Default Expression is Conditional
#'
#' Tests whether expression is conditional by default. Coerces to character.
#' @param x default
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family conditional
#' @return logical
isConditional.default <- function(x,...)isConditional(as.character(x),...)

#' Test Character Expression is Conditional
#'
#' Tests whether character expression is conditional by default.
#' Evaluates x on data and looks for meaningful result.
#' @param x default
#' @param data environment for variable lookup
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family conditional
#' @return logical

isConditional.character <- function(x, data,...){
  nms <- names(x)
  status <- singularity(nms, data, ...)
  if(is.na(status))return(FALSE)
  if(status == 0)return(FALSE)
  TRUE
}

#' Test Value is Levels
#'
#' Tests whether value is levels.
#' @param x character
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family levels
#' @return logical
isLevels <- function(x, ...)UseMethod('isLevels')

#' Test Value is Levels by Default
#'
#' Tests whether value is levels by default.  Coerces to character.
#' @param x default
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family levels
#' @return logical
isLevels.default <- function(x, table, ...)isLevels(as.character(x), table, ...)

#' Test Character Value is Levels
#'
#' Tests whether character value is levels.
#' Looks for any matches to vector.
#' Uses \code{\link{intersect}}, which is fairly flexible
#' respecting underlying data types (character 0 can match integer 0, etc.).
#' @param x default
#' @param table lookup vector
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family levels
#' @return logical

isLevels.character <- function(x, table,  ...){
  as.logical(length(intersect(x,table)) >= 1)
}


