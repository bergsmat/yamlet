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
#
#' For finer control, you can switch between 'data.frame'
#' and 'decorated' using \code{\link{as_decorated}}
#' (supplies null decorations) and \code{\link{as.data.frame}}
#' (preserves decorations).
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
#' # The bare data.frame gives boring labels and unordered groups.
#' map <- aes(x = time, y = conc, color = Heart)
#' data.frame(x) %>% ggplot(map) + geom_point()
#'
#' # Decorated data.frame uses supplied labels.
#' # Notice CHF levels are still not ordered.
#' x %>% ggplot(map) + geom_point()
#'
#' # We can resolve guide for a chance to enrich the output with units.
#' # Notice CHF levels are now ordered.
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


ggplot.decorated <- function(data, ...){
  p <- NextMethod()
  class(p) <- c('decorated_ggplot',class(p))
  p
}
#' Substitute Expressions, Titles and Labels in ggplots
#'
#' At time of printing, default labels will be used as
#' column names to search \code{data} for more meaningful
#' labels, taking first available from attributes
#' with names in \code{search}.
#'
#' @param x class 'decorated_ggplot' from \code{\link{ggplot.decorated}}
#' @param ... passed arguments
#' @param search attribute names from which to seek label substitutes
#' @return see \code{\link[ggplot2]{print.ggplot}}
#' @export
#' @family decorated_ggplot
#' @examples
#' example(ggplot.decorated)

print.decorated_ggplot <- function(
  x,
  ...,
  search = getOption(
    'decorated_ggplot_search',
    c('expression','title','label')
  )
){
  for(i in seq_along(x$labels)){           # x (gg object) stores names of used columns as $labels
    lab <- x$labels[[i]]                   # handle one label
    if(lab %in% names(x$data)){            # if this is just a bare column name
      col <- x$data[[lab]]
      atr <- attributes(col)
      for( s in rev(search)){              # end with first
        label <- atr[[s]]                  # retrieve label
        if(!is.null(label)){
          x$labels[[i]] <- label           # overwrite default label with one from data attributes
        }
      }
      # done with search.  Plural labels?
      if(length(x$labels[[i]]) > 1){
        labs <- x$labels[[i]]
        if(length(names(labs)))labs = paste(
          paste0(
            '(',
            names(labs),
            ')'
          ),
          labs
        )
        labs <- paste(labs, collapse = '\n')
        msg <- paste('using first of', labs, sep = '\n')
        warning(msg)
        x$labels[[i]] <- x$labels[[i]][[1]]
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
#' @param x class 'decorated_ggplot' from \code{\link{ggplot.ggready}}
#' @param ... passed arguments
#' @return see \code{\link[ggplot2]{ggplot_build}}
#' @export
#' @method ggplot_build decorated_ggplot
#' @importFrom ggplot2 ggplot_build
#' @keywords internal
#' @family decorated_ggplot

ggplot_build.decorated_ggplot <- print.decorated_ggplot

#' Determine Scale Type for dvec
#' 
#' Determines scale type for dvec.
#' @param x dvec
#' @export
#' @keywords internal
#' @importFrom ggplot2 scale_type
#' @method scale_type dvec
scale_type.dvec <- function(x)scale_type(unclass(x))

#' Rescale dvec
#' 
#' Rescales dvec
#' @param x dvec
#' @param to numeric
#' @param from numeric
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @importFrom scales rescale
#' @method rescale dvec
rescale.dvec <- function(
    x, 
    to = c(0, 1),
    from = range(x, na.rm = TRUE, finite = TRUE), 
    ...
){
  rescale(unclass(x), to = to, from = from, ...)
}