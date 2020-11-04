#' Create a New ggplot for a GGready Data Frame
#'
#' Creates a new ggplot object for a ggready data.frame.
#' This is the ggplot() method for class 'ggready';
#' it tries to implement automatic labels and units in axes and legends
#' in association with \code{\link{print.dg}}.
#' This approach is deprecated in favor of a cleaner
#' object hierarchy for classes 'decorated' and 'resolved'.
#'
#' @param data data.frame or similar
#' @param ... passed to \code{\link[ggplot2]{ggplot}}
#' @return return value like \code{\link[ggplot2]{ggplot}}
#' @export
#' @importFrom ggplot2 ggplot
#' @family dg
#' @keywords internal
#' @examples
#' meta <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' x <- decorate(meta)
#' library(ggplot2)
#' class(ggplot(data = x) + geom_path(aes(x = time, y = conc)))
#' class(ggplot(data = x, aes(x = time, y = conc)) + geom_path())
#' example(print.dg)

ggplot.ggready <- function(data, ...){
  class(data) <- setdiff(class(data), 'ggready')
  p <- ggplot(data = data, ...)
  class(p) <- c('dg',class(p))
  p
}
#' Print Automatic Labels and Units for ggplot
#'
#' Prints automatic labels and units for ggplot.
#' Substitutes column label, if present, for default.
#'
#' @param x class 'dg' from \code{\link{ggplot.ggready}}
#' @param ... passed arguments
#' @return see \code{\link[ggplot2]{print.ggplot}}
#' @export
#' @keywords internal
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
        x$labels[[i]] <- label             # replace default label with one from data attributes
      }
    }
  }
  NextMethod()
}

#' Enable Automatic Labels and Units for ggplot
#'
#' Enable automatic labels and units for ggplot.
#' Substitutes column label, if present, for default.
#' Supports arrangements of ggplot objects.
#'
#' @param x class 'dg' from \code{\link{ggplot.ggready}}
#' @param ... passed arguments
#' @return see \code{\link[ggplot2]{ggplot_build}}
#' @export
#' @keywords internal
#' @family dg

ggplot_build.dg <- print.dg
