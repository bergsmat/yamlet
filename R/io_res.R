#' Import and Export Resolved Tables
#'
#' Inter-converts between tables as comma-separated variable 
#' and fully resolved data frames.
#' Generic, with character and data.frame methods that extend \code{\link{io_csv}}.

#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @return See methods.
#' @family io
#' @examples
#' example(io_res.character)
io_res <- function(x, ...)UseMethod('io_res')

#' Import Documented Table as Resolved
#'
#' Imports a documented table and resolves ambiguous guide elements.
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
  d <- resolve(d)
  d
}


#' Export Resolved Table
#'
#' Exports a documented table. "Desolves" attributes
#' to standard form, then writes data and metadata to storage.
#' A short-cut for \code{(io_csv(desolve(x))}.
#'
#' @param x decorated; passed to \code{\link{io_csv.data.frame}}
#' @param file passed to \code{\link{io_csv.data.frame}}
#' @param ... passed to \code{\link{io_csv.character}} and \code{\link{desolve.decorated}}
#' @export
#' @keywords internal
#' @family io
#' @family interface
#' @return decorated (invisible)
#' @examples
#' library(magrittr)
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' x <- io_res(file)
#' tmp <- tempfile(fileext = '.csv')
#' io_res(x, tmp)
#' a <- io_csv(tmp, source = FALSE)
#' b <- io_csv(file, source = FALSE)
#' stopifnot(identical(a, b))


io_res.decorated <- function(
    x,
    file = '',
    ...
){
  d <- desolve(x, ...)
  d <- io_csv(d, file = file, ...)
  invisible(d)
}
