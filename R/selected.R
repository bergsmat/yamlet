#' Identify Selected Names
#'
#' Identifies Selected Names.
#' Generic, with default method.
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @return see methods
#' @family modify
#' @examples
#' example(modify.data.frame)
selected <- function(x, ...)UseMethod('selected')

#' Identify Selected Names by Default
#'
#' Evaluates un-named arguments in \code{...} using
#' \code{\link[dplyr]{select}} rules, returning
#' explicit names in x.  Returns all (non-empty) names
#' by default (if no dots supplied).
#'
#' @param x object
#' @param ... to \code{\link[dplyr]{select}}
#' @export
#' @keywords internal
#' @importFrom rlang f_rhs eval_tidy quo_set_env quos
#' @importFrom dplyr select
#' @return character: names in x
#' @family modify
#' @family interface
#' @examples
#' library(magrittr)
#' list(a = 1, b = 1:10, c = letters) %>%
#' selected(b:c)
#'
selected.default <- function(
  x,
  ...
){
  args <- quos(...)
  vars <- args[names(args) == ""]
  y <- names(x) # should work if x has names
  y <- y[y != ''] # ignore empty names
  d <- lapply(y, function(i)character())
  names(d) <- y # reuse names
  d <- data.frame(
    d, 
    check.names = FALSE, 
    fix.empty.names = FALSE
  )# dummy data.frame for dplyr
  vars <- names(select(d,!!!vars))
  if(length(vars) == 0) vars <- y
  # vars <- intersect(vars, names(x))
  class(vars) <- union('selected', class(vars))
  return(vars)
}




