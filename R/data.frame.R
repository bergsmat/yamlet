#' Coerce Yamlet to Data Frame
#'
#' Coerces yamlet to data.frame. Columns are constructed in the order that
#' attributes are encountered, beginning with top-level 'item' (default).
#' Cell contents are calculated using
#' \code{getOption('yamlet_cell_value', yamlet::cell_value)} to which
#' is passed the cell-specific metadata as well as \code{sep} and \code{def}.
#'
#' @param x yamlet; see \code{\link{decorations}} and \code{\link{read_yamlet}}
#' @param row.names a name for a column to hold top-level names, or NULL to represent these as row.names
#' @param optional if TRUE and row.names is NULL, row.names will not be set
#' @param sep separator for multiple items within an attribute
#' @param def definition string: separator between items and their (preceding) names, if any
#' @param ... ignored
#' @export
#' @family interface
#' @return data.frame
#' @examples
#'
#' library(magrittr)
#'
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.yaml')
#' file %>% read_yamlet %>% explicit_guide %>% as.data.frame

#' file <- system.file(package = 'yamlet', 'extdata','phenobarb.csv')
#'
#' # phenobarb.yaml has conditional metadata that benefits
#' # from interpretation in the context of the data itself.
#' # thus, we
#' # * read the whole 'decorated' object (not just yaml),
#' # * resolve the 'guide' ambiguity,
#' # extract the best-guess decorations, and
#' # convert to data.frame.
#'
#' file %>% io_csv %>% resolve %>% decorations %>% as.data.frame
#'
as.data.frame.yamlet <- function(
  x,
  row.names = 'item',
  optional = FALSE,
  sep = '\n',
  def = ': ',
  ...
){
  x <- unclass(x)
  nms <- names(x)
  stopifnot(length(row.names) <= 1)
  stopifnot(length(sep) %in% c(1, length(nms)))
  stopifnot(length(def) %in% c(1, length(nms)))
  if(length(sep) == 1)sep <- rep(sep, length(nms))
  if(length(def) == 1)def <- rep(def, length(nms))
  cols <- lapply(x, names)
  cols <- unlist(cols)
  cols <- unique(cols)
  mat <- matrix(NA, nrow = length(nms), ncol = length(cols))
  fun <- match.fun(getOption('yamlet_cell_value', yamlet::cell_value))
  for(i in seq_along(nms)){
    for(j in seq_along(cols)){
      val <- NA_character_
      nm <- nms[[i]]
      col <- cols[[j]]
      item <- x[[nm]]
      if(col %in% names(item)){
        val <- fun(item[[col]], sep = sep[[i]], def = def[[i]], ...)
      }
      mat[i,j] <- val
    }
  }
  dat <- data.frame(mat)
  names(dat) <- cols
  if(length(row.names)){
    dat[[row.names]] <- nms
    if(ncol(dat) > 1) dat <- dat[, c(ncol(dat), 1:(ncol(dat)-1)),drop = FALSE]
  } else {
    if(!optional) row.names(dat) <- nms
  }
  dat
}

#' Calculate a Cell Value
#'
#' Calculates a cell value.
#'
#' @param x list of character, possibly named
#' @param sep separator for multiple items within an attribute
#' @param def definition string: separator between items and their (preceding) names, if any
#' @export
#' @keywords internal
#'
cell_value <- function(x, sep = '\n', def = ': '){
  nms <- names(x)
  y <- sapply(x, paste, collapse = '') # guarrantee length one character
  if(!is.null(nms)) y <- paste0(nms, def, y)
  z <- paste(y, collapse = sep)
  z
}







