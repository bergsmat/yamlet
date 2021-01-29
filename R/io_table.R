#' Import and Export Documented Tables
#'
#' Imports or exports documented tables.  Generic, with methods
#' that extend \code{\link{read.table}} and \code{\link{write.table}}.

#'@param x object
#'@param ... passed arguments
#'@export
#'@return See methods.
#'@family io
#'@examples
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' x <- decorate(file)
#' out <- file.path(tempdir(), 'out.tab')
#' foo <- io_table(x, out)
#' identical(out, foo)
#' y <- io_table(foo, as.is = TRUE)
#' attr(x, 'source') <- NULL
#' rownames(x) <- NULL
#' rownames(y) <- NULL
#' identical(x, y) # lossless 'round-trip'
io_table <- function(x, ...)UseMethod('io_table')

#' Import Documented Table
#'
#' Imports a documented table.
#' A wrapper for read.table() that also
#' reads associated yamlet metadata, if present, and applies it
#' as attributes.
#'
#' @param x character file path; passed to \code{\link{read.table}}
#' @param ext extension for metadata equivalent of x
# @param coerce whether to coerce to factor where guide is a list; passed to \code{\link{decorate.data.frame}}
#' @param ... passed to \code{\link{read.table}} (if accepted) and to \code{\link{decorate}}
#' @export
#' @keywords internal
#' @family io
#' @family interface
#' @return data.frame
#' @examples
#' example(io_table)
io_table.character <- function(
  x,
  ext = getOption('yamlet_extension', '.yaml'),
  #coerce = getOption('yamlet_coerce', FALSE),
  ...
){
  args <- list(...)
  args <- args[names(args) %in% names(formals(utils::read.table))]
  args <- c(list(file = x), args)
  d <- do.call(utils::read.table, args)
  meta <- sub('\\.[^.]*$','',x) # remove last dot and any trailing chars
  meta <- paste0(meta, ext)
  if(!file.exists(meta)){
    message('did not find ', meta)
  }else{
    d <- decorate(
      d,
      meta = meta,
     # coerce = coerce,
      ...
    )
  }
  d
}

#' Export Documented Table
#'
#' Exports a data.frame and a yamlet version of its decorations.
#' A wrapper for \code{\link{write.table}}.
#'
#' @param x data.frame
#' @param file passed to \code{\link{write.table}}
#' @param ext = extension for metadata equivalent of x
# @ 0.6.1, dropping coerce (passed to io_yamlet, which does not implement)
# @param coerce logical; whether to coerce factor levels to guide; alternatively, a key for the levels
#' @param con passed to \code{\link{io_yamlet}}
#' @param useBytes passed to \code{\link{io_yamlet}}
#' @param default_keys passed to \code{\link{io_yamlet}}
#' @param ... passed to \code{\link{write.table}} (if accepted) and to \code{\link{io_yamlet}}
#' @export
#' @keywords internal
#' @family io
#' @family interface
#' @return invisible(file)
#' @examples
#' example(io_table)
io_table.data.frame <- function(
  x,
  file = '',
  ext = getOption('yamlet_extension', '.yaml'),
  # coerce = getOption("yamlet_coerce_decorations", FALSE),
  con = stdout(),
  useBytes = FALSE,
  default_keys = getOption(
    'yamlet_default_keys',
    list('label','guide')
  ),
  ...
){
  args <- list(...)
  args <- args[names(args) %in% names(formals(utils::write.table))]
  args <- c(list(x = x, file = file),args)
  do.call(utils::write.table, args)
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
   # coerce = coerce,
    ...
  )
  invisible(file)
}
