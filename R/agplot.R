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
#' Coerces list to axis label.
#'
#' @param x list, such as returned by \code{\link{attributes}}.
#' @param default a value to return by default
#' @param collapse character: separator for collapsing multi-line units
#' @param enclose length-two character for enclosing unit
#' @param ... ignored
#' @return length-one character
#' @export
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
  enclose = getOption('enclose', default = c('(',')')),
  ...
){
  stopifnot(length(default) == 1, is.character(default))
  stopifnot(length(enclose) == 2, is.character(enclose))
  out <- default
  if('label' %in% names(x)) out <- x$label
  more <- character(0)
  if('units' %in% names(x)) more <- x$units
  if('unit' %in% names(x)) more <- x$unit
  if('guide' %in% names(x)){
    if(length(x$guide) == 1) more <- x$guide
  }
  if(length(more) > 1) more <- paste(more, collapse = collapse)
  if(length(more)) more <- paste0(enclose[[1]], more, enclose[[2]])
  out <- paste(out, more)
  out
}

#' Request Automatic Labels and Units for ggplot
#'
#' Requests automatic labels and units for ggplot.
#' Simply subclasses the output of ggplot, in
#' expectation of associated print method \code{\link{print.ag}}.
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
#' class(agplot(data = x) + geom_path(aes(x = time, y = conc)))
#' class(agplot(data = x, aes(x = time, y = conc)) + geom_path())
#' example(print.ag)

agplot <- function(data, ...){
  p <- ggplot(data = data, ...)
  class(p) <- c('ag',class(p))
  p
}
#' Print Automatic Labels and Units for ggplot
#'
#' Prints automatic labels and units for ggplot.
#' Reworks the labels as a function of attributes
#' in corresponding data. \code{labeller} will
#' receive existing labels one at a time
#' and corresponding attributes(if any) from data.
#'
#' @param x class 'ag' from \code{\link{agplot}}
#' @param labeller a function (or its name) like \code{\link{as_lab}} to generate axis labels
#' @param ... passed arguments
#' @return used for side effects
#' @export
#' @family lab
#' @examples
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' x <- decorate(file, coerce = TRUE )
#' library(ggplot2)
#' agplot(data = x) + geom_point(aes(x = time, y = conc, color = Heart))
#' agplot(data = x, aes(x = time, y = conc)) + geom_point()
#' agplot(data = x) + geom_point(aes(x = time, y = conc)) + xlab('the time (hours)')
#' options(enclose = c('[',']'))
#' agplot(data = x) + geom_point(aes(x = time, y = conc, color = Creatinine))


print.ag <- function(x, labeller = getOption('labeller', default = as_lab), ...){
  fun <- match.fun(labeller)
  for(i in seq_along(x$labels)){
    lab <- x$labels[[i]]
    if(lab %in% names(x$data)){
      attr <- attributes(x$data[[lab]])
      if(!is.null(attr)){
        val <- fun(x = attr, default = lab, ...)
        x$labels[[i]] <- val
      }
    }
  }
  NextMethod()
}
