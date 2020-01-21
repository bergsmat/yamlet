#' Import and Export Documented Tables as CSV
#'
#' Imports or exports documented tables as comma-separated variable.
#' Generic, with methods that extend \code{\link[csv]{as.csv}}.

#'@param x object
#'@param ... passed arguments
#'@export
#'@return See methods.
#'@family io
#'@examples
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' x <- decorate(file)
#' out <- file.path(tempdir(), 'out.csv')
#' foo <- io_csv(x, out)
#' identical(out, foo)
#' y <- io_csv(foo)
#' attr(x, 'source') <- NULL
#' attr(y, 'source') <- NULL
#' identical(x, y) # lossless 'round-trip'
io_csv <- function(x, ...)UseMethod('io_csv')

#' Import Documented Table as CSV
#'
#' Imports a documented table as comma-separated variable.
#' A wrapper for \code{\link[csv]{as.csv.character}} that also
#' reads associated yamlet metadata, if present, and applies it
#' as attributes.
#'
#' @param x character file path; passed to \code{\link[csv]{as.csv.character}} (by method dispatch)
#' @param ext extension for metadata equivalent of x
# @param coerce whether to coerce to factor where guide is a list; passed to \code{\link{decorate.data.frame}}
#' @param ... passed to \code{\link[csv]{as.csv.character}} and to \code{\link{decorate}}
#' @export
#' @keywords internal
#' @importFrom csv as.csv
#' @family io
#' @family interface
#' @return data.frame
#' @examples
#' example(io_csv)
io_csv.character <- function(
  x,
  ext = getOption('yamlet_extension', '.yaml'),
  #coerce = getOption('yamlet_coerce', FALSE),
  ...
){
  d <- csv::as.csv(x, ...)
  meta <- sub('\\.[^.]*$','',x) # remove last dot and any trailing chars
  meta <- paste0(meta, ext)
  if(!file.exists(meta)){
    message('did not find ', meta)
  }else{
    d <- decorate(
      d,
      meta = meta,
      #coerce = coerce,
      ...)
  }
  d
}

#' Export Documented Table as CSV
#'
#' Exports a data.frame as comma-separated variable,
#' as well as a yamlet version of its decorations.
#' A wrapper for \code{\link[csv]{as.csv.data.frame}}.
#'
#' @param x data.frame
#' @param file passed to \code{\link[csv]{as.csv.data.frame}} (by method dispatch)
#' @param ext = extension for metadata equivalent of x
#' @param coerce logical; whether to coerce factor levels to guide; alternatively, a key for the levels
#' @param con passed to \code{\link{io_yamlet}}
#' @param useBytes passed to \code{\link{io_yamlet}}
#' @param default_keys passed to \code{\link{io_yamlet}}
#' @param ... passed to \code{\link{as.csv}} and to \code{\link{io_yamlet}}
#' @export
#' @keywords internal
#' @importFrom csv as.csv
#' @family io
#' @family interface
#' @return invisible(file)
#' @examples
#' example(io_table)
io_csv.data.frame <- function(
  x,
  file = '',
  ext = getOption('yamlet_extension', '.yaml'),
  coerce = getOption("yamlet_coerce_decorations", FALSE),
  con = stdout(),
  useBytes = FALSE,
  default_keys = getOption(
    'yamlet_default_keys',
    list('label','guide')
  ),
  ...
){
  csv::as.csv(x, file = file, ...)
  if(is.character(file)){
    if(file != ''){
      con <- sub('\\.[^.]*$','',file) # remove last dot and any trailing chars
      con <- paste0(con, ext)
    }
  }
  io_yamlet(
    x,
    con = con,
    useBytes = useBytes,
    default_keys = default_keys,
    coerce = coerce,
    ...
  )
  invisible(file)
}
