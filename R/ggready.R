#' Prepare Data for GGplot
#'
#' Prepares data for ggplot.
#' Generic, with methods for data.frame, decorated, and resolved.
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @return see methods
#' @family ggready
#' @examples
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' x <- decorate(file)
#' decorations(x, Weight)
#' decorations(as.data.frame(x), Weight) # downgrade still has attributes
#' class(x)
#' class(ggready(as.data.frame(x)))
#' class(ggready(x))
#' class(ggready(resolve(x)))
#' x <- ggready(x)
#' library(magrittr)
#' library(ggplot2)
#'
#' # Here we filter on-the-fly
#' # without loss of attributes.
#' # Notice mg/L rendering; this is actually part of an expression.
#' file %>%
#'  decorate %>%
#'  filter(!is.na(conc)) %>%
#'  ggready %>%
#'  ggplot(aes(x = time, y = conc, color = Heart)) +
#'  geom_point()
#'
#' # By default ggready resolves everything decorated.
#' # But we can intervene to resolve selectively,
#' # And further intervene to 'ggready' selectively.
#' #
#' x <- file %>% decorate %>% filter(!is.na(conc))
#' x %>%
#' resolve(conc, time) %>%   # Heart left unresolved!
#' ggready(conc, Heart) %>%  # time left unreadied!
#' ggplot(aes(x = time, y = conc, color = Heart)) + geom_point()
#'
#' # Still, all the labels were actually expressions:
#' x %>%
#' resolve(conc, time) %>%
#' ggready(conc, Heart) %>%
#' decorations(conc, time, Heart)

ggready <- function(x, ...)UseMethod('ggready')

#' Prepare Data Frame for GGplot
#'
#' Prepares data.frame for ggplot. Appends
#' units to label using \code{\link{append_units}}
#' (passing \code{style = 'plotmath'} if \code{parse}
#' is true, else \code{style = 'plain'}).
#' Enforces class 'ggready'.
#'
#' @param x object
#' @param ... passed to \code{\link{append_units}}; may include unquoted column names
#' @param parse passed to \code{\link{append_units}}
#' @export
#' @importFrom spork as_spork
#' @importFrom spork plotmathToken
#' @return ggready
#' @keywords internal
#' @family ggready
#' @examples
#' example(ggready)

ggready.data.frame <- function(
  x, ... ,
  parse = getOption('ggready_parse',TRUE)
){
  stopifnot(is.logical(parse), length(parse) == 1)
  x <- append_units(x, ..., style = if(parse) 'plotmath' else 'plain')
  class(x) <- union('ggready', class(x))
  x
}
#' Prepare Decorated Data Frame for GGplot
#'
#' Prepares decorated data.frame for ggplot. Calls
#' \code{\link{resolve}} and appends
#' units to label using \code{\link{append_units}}
#' (passing \code{style = 'plotmath'} if \code{parse}
#' is true, else \code{style = 'plain'}).
#' Enforces classes 'decorated','resolved', and  'ggready'.
#'
#' @param x object
#' @param ... passed to \code{\link{append_units}} and \code{\link{resolve}}; may include unquoted column names
#' @param parse passed to \code{\link{append_units}}
#' @export
#' @importFrom spork as_spork
#' @importFrom spork plotmathToken
#' @return ggready
#' @keywords internal
#' @family ggready
#' @examples
#' example(ggready)
ggready.decorated <- function(
  x, ... ,
  parse = getOption('ggready_parse',TRUE)
){
  x <- resolve(x, ...)
  stopifnot(is.logical(parse), length(parse) == 1)
  x <- append_units(x, ..., style = if(parse) 'plotmath' else 'plain')
  class(x) <- union('decorated', class(x))
  class(x) <- union('resolved', class(x))
  class(x) <- union('ggready', class(x))
  x
}
#' Prepare Resolved Data Frame for GGplot
#'
#' Prepares resolved data.frame for ggplot. Appends
#' units to label using \code{\link{append_units}}
#' (passing \code{style = 'plotmath'} if \code{parse}
#' is true, else \code{style = 'plain'}).
#' Enforces classes 'decorated','resolved', and  'ggready'.
#' Unlike \code{\link{ggready.decorated}}, the
#' method for class resolved does NOT call resolve(),
#' and so does not second-guess any particular
#' resolutions you may have already made.
#'
#' @param x object
#' @param ... passed to \code{\link{append_units}} and \code{\link{resolve}}; may include unquoted column names
#' @param parse passed to \code{\link{append_units}}
#' @export
#' @importFrom spork as_spork
#' @importFrom spork plotmathToken
#' @return ggready
#' @keywords internal
#' @family ggready
#' @examples
#' example(ggready)
ggready.resolved <- function(
  x, ... ,
  parse = getOption('ggready_parse',TRUE)
){
  stopifnot(is.logical(parse), length(parse) == 1)
  x <- append_units(x, ..., style = if(parse) 'plotmath' else 'plain')
  class(x) <- union('decorated', class(x))
  class(x) <- union('resolved', class(x))
  class(x) <- union('ggready', class(x))
  x
}

#' @export
spork::plotmathToken
