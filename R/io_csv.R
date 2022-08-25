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
#' # generate some decorated data
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' x <- decorate(file)
#' 
#' # get a temporary filepath
#' out <- file.path(tempdir(), 'out.csv')
#' 
#' # save file using io_csv (returns filepath)
#' foo <- io_csv(x, out)
#' stopifnot(identical(out, foo))
#' 
#' # read using this filepath
#' y <- io_csv(foo)
#' 
#' # lossless round-trip (ignoring source attribute)
#' attr(x, 'source') <- NULL
#' attr(y, 'source') <- NULL
#' stopifnot(identical(x, y))
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
#' @param meta explicit file path for metadata; if null, \code{ext} is appended to x after removing (final) extension, if any
#' @param gz logical; guessed by default from \code{x}; if TRUE, '.gz' extension enforced present for \code{x} and absent for default \code{meta}
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
  meta = NULL,
  gz = NULL,
  #coerce = getOption('yamlet_coerce', FALSE),
  ...
){
  hasGZ <- grepl(ignore.case = TRUE, pattern = '\\.gz$', x)
  if(is.null(gz)) gz <- hasGZ
  stopifnot(length(gz) == 1, is.logical(gz))
  if(gz & !hasGZ) x <- paste0(x, '.gz')
  d <- csv::as.csv(x, ...)
  if(is.null(meta)){
    meta <- x
    if(gz) meta <- sub('\\.gz$', '', meta, ignore.case = TRUE)
    meta <- sub('\\.[^.]*$', '', meta) # remove last dot and any trailing chars
    meta <- paste0(meta, ext)
  }
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
#' You should be able to supply exactly the connections you want for 
#' \code{file} (the data file) and \code{meta} (the metadata file)
#' if \code{gz} is FALSE. If \code{gz} is NULL, it will be 
#' guessed from file (TRUE for character ending with '.gz' or '.GZ').
#' If TRUE, character \code{file} will have '.gz' extension enforced,
#' but any '.gz' ('.GZ') will be stripped when calculating \code{meta}.
#' 
#'
#' @param x data.frame
#' @param file passed to \code{\link[csv]{as.csv.data.frame}} (by method dispatch)
#' @param ext = extension for metadata equivalent of x
# coerce was passed to io_yamlet, which does not implement it
# @param coerce logical; whether to coerce factor levels to guide; alternatively, a key for the levels
#' @param meta passed as \code{con} to \code{\link{io_yamlet}}
#' @param gz logical; guessed by default from \code{x}; if TRUE, '.gz' extension enforced present for (character) \code{file} and absent for default \code{meta}
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
#' example(io_csv)
io_csv.data.frame <- function(
  x,
  file = '',
  ext = getOption('yamlet_extension', '.yaml'),
  # coerce = getOption("yamlet_coerce_decorations", FALSE),
  meta = stdout(),
  gz = NULL,
  useBytes = FALSE,
  default_keys = getOption(
    'yamlet_default_keys',
    list('label','guide')
  ),
  ...
){
  hasGZ <- grepl(ignore.case = TRUE, pattern = '\\.gz$', file)
  isSTDOUT <- identical(summary(meta), summary(stdout()))
  if(is.null(gz)) gz <- hasGZ
  stopifnot(length(gz) == 1, is.logical(gz))
  stopifnot(length(file) == 1)
  # Now gz is true or false.  
  # calculate file
  if(is.character(file) & file != '' & gz & !hasGZ) file <- paste0(file, '.gz')
  
  # calculate meta
  if(is.character(file) & file != '' & isSTDOUT){  
    # i.e., user has supplied file as character, but has not supplied meta
    meta <- file
    if(gz) meta <- sub('\\.gz$', '', meta, ignore.case = TRUE)
    meta <- sub('\\.[^.]*$', '', meta) # remove last dot and any trailing chars
    meta <- paste0(meta, ext)
  }
  
  # if file is character and gz is true, convert it to connection
  was <- file
  if(is.character(file) & file != '' & gz) file <- gzfile(file)
  csv::as.csv(x, file = file, ...)

  io_yamlet(
    x,
    con = meta,
    useBytes = useBytes,
    default_keys = default_keys,
    # coerce = coerce,
    ...
  )
  invisible(was)
}
