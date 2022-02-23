#' Gather a Decorated Data Frame
#'
#' Gathers a decorated data.frame.
#' I.e. a gather method for class 'decorated'.
#' Invokes tidyr::gather(), converting gathered
#' labels to the guide attribute of \code{key}, and
#' converting gathered guides ... if all the same ...
#' to the guide attribute of \code{value}.
#' Somewhat experimental!
#'
#' @param data see \code{\link[tidyr]{gather}}
#' @param key see \code{\link[tidyr]{gather}}
#' @param value see \code{\link[tidyr]{gather}}
#' @param ... see \code{\link[tidyr]{gather}}
#' @param na.rm see \code{\link[tidyr]{gather}}
#' @param convert see \code{\link[tidyr]{gather}}
#' @param factor_key see \code{\link[tidyr]{gather}}
#' @export
#' @importFrom dplyr select
#' @importFrom tidyr gather
#' @importFrom rlang ensym
#' @return decorated
#' @keywords internal
#' @examples
#' library(magrittr)
#' library(tidyr)
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' x <- decorate(file)
#' x %>% gather('key', 'value', time, interval) %>% decorations
#'

gather.decorated <- function(
  data,
  key = 'key',
  value = 'value',
  ...,
  na.rm = FALSE,
  convert = FALSE,
  factor_key = FALSE
){
  # NextMethod()
  # @ tidyr 1.1.2, NextMethod() i.e. tidyr.data.frame does not respect key/value.
  args <- quos(...)
  args <- args[names(args) == ""]
  if(length(args) == 0) return(data)
  #if(length(args[[1]] == 0)) return(data)
  class(data) <- setdiff(class(data), 'decorated')
  x <- gather(
    data = data,
    key = !!key,
    value = !!value,
    ...,
    na.rm = na.rm,
    convert = convert,
    factor_key = factor_key
  )
  x <- ungroup(x) # @0.4.9, to select only one column without autoselection of groups
  if(key %in% names(x)){
    token <- names(select(x, !!key))
    val <- names(select(x, !!value))
    nms <- unique(x[[token]])
    labs <- sapply(nms, function(nm)attr(data[[nm]],'label'))
    names(nms) <- labs
    attr(x[[token]], 'guide') <- nms
    guides <- lapply(nms, function(nm)attr(data[[nm]],'guide'))
    guides <- unique(guides)
    if(length(guides) == 1)attr(x[[val]], 'guide') <- guides[[1]]
  }
  as_decorated(x)
}
