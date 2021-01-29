#' Import Resolved Tables
#'
#' Imports tables as comma-separated variable and resolves ambiguous guide elements.
#' Generic, with character method that extends \code{\link{io_csv}}.

#'@param x object
#'@param ... passed arguments
#'@export
#'@return See methods.
#'@family io
#'@examples
#' example(io_res.character)
io_res <- function(x, ...)UseMethod('io_res')

#' Import Documented Table as Resolved
#'
#' Imports a documented table resolve ambiguouous guide elements.
#' A wrapper for \code{\link{io_csv.character}} that also
#' reads associated yamlet metadata, if present, and applies it
#' as attributes. Invokes \code{\link{resolve}} to resolve
#' ambiguity of 'guide' attribute, if possible. A short-cut
#' for \code{resolve(io_csv(x))}.
#'
#' @param x character file path; passed to \code{\link{io_csv.character}}
#' @param ext extension for metadata equivalent of x
#' @param ... passed to \code{\link{io_csv.character}}
#' @export
#' @keywords internal
#' @family io
#' @family interface
#' @return decorated
#' @examples
#' library(magrittr)
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' x <- io_csv(file) %>% resolve
#' y <- io_res(file)
#' identical(x, y)


io_res.character <- function(
  x,
  ext = getOption('yamlet_extension', '.yaml'),
  ...
){
  d <- io_csv(x = x, ext = ext, ...)
  d<- resolve(d)
  d
}
