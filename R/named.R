#' Evaluate Named Arguments
#'
#' Evaluates named arguments in \code{...}.
#'
#' @param ... possibly a mix of named and un-named arguments.
#' @export
#' @importFrom rlang eval_tidy quos
#' @importFrom dplyr select
#' @return named list
#' @family modify
#' @keywords internal
#' @examples
#' named(a = 1, b = 2 + 3, 4, 'd')
#'
named <- function(...){
  args <- quos(...)
  args <- args[names(args) != ""]
  args <- lapply(args, rlang::eval_tidy)
  args
}




