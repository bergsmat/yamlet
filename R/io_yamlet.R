#' Import and Export Yamlet
#'
#' Imports and exports yamlet.
#' Generic, with a read method \code{\link{read_yaml}}
#' for character and a write method \code{\link{write_yaml}}
#' for data.frame.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @return see methods
#' @family io
#' @family interface
#' @examples
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.yaml')
#' x <- io_yamlet(file)
#' tmp <- tempdir()
#' out <- file.path(tmp, 'tmp.yaml')
#'
#' # we can losslessly 'round-trip' x using to generic calls
#' identical(x, io_yamlet(io_yamlet(x, out)))

io_yamlet <- function(x, ...)UseMethod('io_yamlet')

#' Import Yamlet
#'
#' Imports yamlet.
#' Character method for \code{\link{io_yamlet}}.
#' Similar to \code{\link{read_yamlet}}, but only reads files.
#'
#' @param x file path for yamlet
#' @param default_keys character: default keys for the first n anonymous members of each element
#' @param ... passed to \code{\link{as_yamlet}}
#' @export
#' @keywords internal
#' @family io
#' @family interface
#' @seealso \code{\link{decorate.list}}
#' @return yamlet: a named list with default keys applied
#' @examples
#' example(io_yamlet)
io_yamlet.character <- function(
  x,
  default_keys = getOption(
    'yamlet_default_keys',
    list('label','guide')
  ),
  ...
){
  stopifnot(file.exists(x))
  as_yamlet(x, default_keys = default_keys, ...)
}

#' Export Yamlet
#'
#' Exports yamlet.
#' The archtype method for \code{\link{io_yamlet}}.
#' Similar to \code{\link{write_yamlet}} but returns (description of) \code{con}.
#'
#' @param x yamlet
#' @param con passed to \code{\link{writeLines}}
#' @param eol end-of-line; passed to \code{\link{writeLines}} as \code{sep}
#' @param useBytes passed to \code{\link{writeLines}}
#' @param default_keys character: default keys for the first n anonymous members of each element
#' @param fileEncoding if \code{con} is character, passed to \code{\link{file}} as \code{encoding}
#' @param ... passed to \code{\link{as_yamlet}}
#' @export
#' @keywords internal
#' @family interface
#' @family io
#' @return invisible description of con: i.e., a file path
#' @examples
#' library(csv)
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' x <- decorate(file)
#' out <- file.path(tempdir(), 'out.yamlet')
#' io_yamlet(as_yamlet(x), out)
#' io_yamlet(out)
#'
io_yamlet.yamlet <- function(
  x,
  con = stdout(),
  eol = "\n",
  useBytes = FALSE,
  default_keys = getOption(
    'yamlet_default_keys',
    list('label','guide')
  ),
  fileEncoding = getOption('encoding'),
  ...
){
  y <- as.character(x, default_keys = default_keys, ...)

  if(is.character(con)){
    con <- file(con, 'w', encoding = fileEncoding)
    on.exit(close(con))
  }
  writeLines(text = y, con = con, sep = eol, useBytes = useBytes)
  invisible(summary(con)$description)
}

#' Export Data Frame Attributes as Yamlet
#'
#' Writes data.frame attributes as yamlet.
#' The data.frame method for \code{\link{io_yamlet}}.
#' Similar to \code{\link{write_yamlet}}, but returns (description of) \code{con}.
#'
#' @param x data.frame
#' @param con passed to \code{\link{writeLines}}
#' @param eol end-of-line; passed to \code{\link{writeLines}} as \code{sep}
#' @param useBytes passed to \code{\link{writeLines}}
#' @param default_keys character: default keys for the first n anonymous members of each element
#' @param fileEncoding if \code{con} is character, passed to \code{\link{file}} as \code{encoding}
#' @param ... passed to \code{\link{as_yamlet}}
#' @export
#' @keywords internal
#' @family interface
#' @family io
#' @return invisible character representation of yamlet (storage syntax)
#' @examples
#' library(csv)
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' x <- decorate(file)
#' out <- file.path(tempdir(), 'out.yamlet')
#' io_yamlet(x, out)
#' io_yamlet(out)
#'
io_yamlet.data.frame <- function(
  x,
  con = stdout(),
  eol = "\n",
  useBytes = FALSE,
  default_keys = getOption(
    'yamlet_default_keys',
    list('label','guide')
  ),
  fileEncoding = getOption('encoding'),
  ...
){
  x <- as_yamlet(x, ...)
  y <- as.character(x, default_keys = default_keys, ...)
  if(is.character(con)){
    con <- file(con, 'w', encoding = fileEncoding)
    on.exit(close(con))
  }
  writeLines(text = y, con = con, sep = eol, useBytes = useBytes)
  invisible(summary(con)$description)
}
