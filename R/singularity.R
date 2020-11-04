#' Choose Singular Expression
#'
#' For a list of expressions evaluated on a data.frame
#' this returns the index of the one expression that evaluates
#' to an all-true vector (after coercing NA to FALSE).
#' Returns 0 if no expressions succeed, and NA_integer_ if
#' more than one succeed. Returns -1 if any expression
#' does not evaluate to logical or if list is empty.
#'
#' @param x list of expressions
#' @param data data.frame
#' @param ... ignored
#' @export
#' @keywords internal
#' @return integer, possibly NA
#' @family lab
#' @examples
#' meta <- system.file(package = 'yamlet', 'extdata','phenobarb.csv')
#' x <- read.csv(meta)
#' singularity(
#'   data = x,
#'   list(
#'     "event == 'conc'",
#'     "event == 'dose'",
#'     "event == 'metabolite'"
#'   )
#' )
#' singularity(
#'   data = x[x$event == 'dose',],
#'   list(
#'     "event == 'conc'",
#'     "event == 'dose'",
#'     "event == 'metabolite'"
#'   )
#' )
#' singularity(
#'   data = x[x$event == 'dose',],
#'   list(
#'     "time >= 0",
#'     "event == 'dose'"
#'   )
#' )
#
singularity <- function(x, data, ...){
  if(!length(x))return(-1)
  #exprs <- lapply(x, function(i)parse(text = i))
  #vals <- lapply(exprs, function(i)try(eval(i, envir = data, enclos = NULL)))
  vals <- lapply(
    x, function(i)try(
      silent = TRUE,
      eval(
        parse(text = i),
        envir = data,
        enclos = NULL
      )
    )
  )
  defined <- lapply(vals, function(i){
    if(inherits(i, 'try-error')) return(-1) # i <- FALSE
    if(!is.logical(i)) return(-1) # i <- as.logical(i)
    i[is.na(i)] <- FALSE
    i
  })
  condensed <- sapply(defined, all)
  res <- sum(condensed)
  if(res == 0) return(as.integer(res))
  if(res > 1) return(NA_integer_)
  # res = 1
  res <- seq_along(condensed)[condensed]
  stopifnot(length(res) == 1)
  res
}


