

#' Choose Singular Expression
#'
#' For a list of expressions evaluated on a data.frame
#' this returns the index of the one expression that evaluates
#' to an all-true vector (after coercing NA to FALSE).
#' Returns 0 if no expressions succeed, and NA_integer_ if
#' more than one succeed. Returns -1 if any expression
#' does not evaluate to logical or if list is empty.
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
  if(!length(x))return(-1)
  #exprs <- lapply(x, function(i)parse(text = i))
  #vals <- lapply(exprs, function(i)try(eval(i, envir = data, enclos = NULL)))
  vals <- lapply(
    x, function(i)try(
      silent = TRUE,
      eval(
        parse(text = i),
        envir = data,
        enclos = NULL
      )
    )
  )
  defined <- lapply(vals, function(i){
    if(inherits(i, 'try-error')) return(-1) # i <- FALSE
    if(!is.logical(i)) return(-1) # i <- as.logical(i)
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
#' in association with \code{\link{print.dg}}.
#' Use \code{ggplot(as.data.frame(x))} to get default
#' ggplot() behavior. Use \code{ggplot(as_decorated(x))}
#' to enforce custom behavior.
#'
#' @param data data.frame or similar
#' @param ... passed to \code{\link[ggplot2]{ggplot}}
#' @return return value like \code{\link[ggplot2]{ggplot}}
#' @export
#' @importFrom ggplot2 ggplot
#' @family dg
#' @family interface
#' @examples
#' meta <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' x <- decorate(meta)
#' library(ggplot2)
#' class(ggplot(data = x) + geom_path(aes(x = time, y = conc)))
#' class(ggplot(data = x, aes(x = time, y = conc)) + geom_path())
#' example(print.dg)

ggplot.decorated <- function(data, ...){
  class(data) <- setdiff(class(data), 'decorated')
  p <- ggplot(data = data, ...)
  class(p) <- c('dg',class(p))
  p
}
#' Print Automatic Labels and Units for ggplot
#'
#' Prints automatic labels and units for ggplot.
#' Substitutes column label, if present, for default.
#'
#' @param x class 'dg' from \code{\link{ggplot.decorated}}
#' @param ... passed arguments
#' @return see \code{\link[ggplot2]{print.ggplot}}
#' @export
#' @family dg
#' @examples
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' library(ggplot2)
#' library(dplyr)
#' library(magrittr)
#' # par(ask = FALSE)
#' options(yamlet_enclose = c('[ ',' ]'))
#'
#' # resolve() promotes factors to a class
#' # that retains attributes when subsetting,
#' # so legend has access to the label from Heart,
#' # even after a filter operation.
#'
#' file %>% decorate %>% resolve %>% filter(!is.na(conc)) %>%
#' ggplot(aes(x = time, y = conc, color = Heart)) + geom_point()
#'
#' # No factors created here, but print.dg promotes guide to factor if it can:
#'
#' file %>% decorate %>% filter(!is.na(conc)) %>%
#' ggplot(aes(x = time, y = conc, color = Heart)) + geom_point()
#'
#' # facet_wrap() should use decodes where available.
#' # resolve() makes them available by promoting to
#' # (a subclass of) factor.
#'
#' file %>% decorate %>% filter(!is.na(conc)) %>% resolve %>%
#' ggplot(aes(x = time, y = conc)) + geom_point() + facet_wrap(~Creatinine)
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


print.dg <- function(x, ...){
  # fun <- match.fun(labeller)
  for(i in seq_along(x$labels)){           # x (gg object) stores names of used columns as $labels
    lab <- x$labels[[i]]                   # handle one label

    if(lab %in% names(x$data)){            # if this is just a bare column name
      col <- x$data[[lab]]
      atr <- attributes(col)
      label <- atr$label                   # retrieve label
      if(!is.null(label)){
        x$labels[[i]] <- label             # replace default label with one from labeller
      }
    }
  }
  NextMethod()
}


