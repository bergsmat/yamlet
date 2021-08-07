#' Choose Singular Expression
#'
#' For a list of expressions evaluated within data,
#' this returns the index of the one expression that evaluates
#' to an all-true vector (after coercing NA to FALSE).
#' Returns 0 if no expressions succeed, and NA_integer_ if
#' more than one succeed. Returns -1 if any expression
#' does not evaluate to logical or if list is empty.
#'
#' @param x list of expressions
#' @param data something coercible to a data environment (typically data.frame)
#' @param ... ignored
#' @export
#' @keywords internal
#' @return integer, possibly NA
#' @family promote
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
  defined <- lapply(vals, function(i){ # must return logical
    if(inherits(i, 'try-error')) return(FALSE) # i <- FALSE
    if(!is.logical(i)) return(FALSE) # i <- as.logical(i)
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

#' Promote Something
#'
#' Promotes something.  Generic, with default method.
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @return see methods
#' @family promote
#' @examples
#' example(promote.data.frame)
promote <- function(x, ...)UseMethod('promote')

#' Promote by Default
#'
#' Promotes by default.  Currently a non-operation.
#'
#' @param x object
#' @param ... other arguments
#' @export
#' @keywords internal
promote.default <- function(x, ...)x

#'
#' Promote a List
#'
#' Promotes attributes of list-like objects.
#' For the plural attributes of each element,
#' any singularity is promoted to the sole attribute.
#' Reserved attributes are untouched.
#' Methods \code{\link{filter.decorated}} and \code{\link{[.decorated}}
#' automatically attempt to promote attributes for all elements.
#' @param x list, or list-like
#' @param ... indicated elements
#' @param .reserved attributes to leave untouched
#' @export
#' @keywords internal
#' @importFrom rlang f_rhs eval_tidy quo_set_env quos
#' @return same class as x
#' @family promote
#' @family interface
#' @seealso filter.decorated [.decorated
#' @examples
#' library(magrittr)
#' file <- system.file(package = 'yamlet', 'extdata','phenobarb.csv')
#' x <- file %>% decorate
#'
#' # Note that there are two elements each for value label and value guide.
#' x %>% decorations(event, value)
#'
#' # After filtering, only one set is relevant.
#' # promote() identifies and retains such.
#' x %>% dplyr:::filter.data.frame(event == 'dose') %>% decorations(value)
#' x %>% dplyr:::filter.data.frame(event == 'dose') %>% promote %>% decorations(value)
#'
#' # If for some reason we do a partial promote, value attributes are unaffected.
#' # Nonsense example:
#' x %>% dplyr:::filter.data.frame(event == 'dose') %>% promote(event) %>% decorations(value)
#'
#' # However, the 'decorated' method for filter() calls promote() internally.
#' x %>% filter(event == 'dose') %>% decorations(value)
#'

promote.list <- function(
  x,
  ...,
  .reserved = getOption(
    'yamlet_promote_reserved',
    c('class','levels','labels','names')
  )
){
  stopifnot(is.character(.reserved))
  vars <- selected(x, ...)
  for(var in vars){
    attr <- attributes(x[[var]])
    nms <- names(attr)
    nms <- nms[nms != '']
    nms <- setdiff(nms, .reserved)
    for(nm in nms){
      this <- attr[[nm]]
      cond <- names(this)
      if(!is.null(cond)){ # only meaningful for attributes whose values have names
        verdict <- singularity(cond, x)
        if(!is.na(verdict)){
          if(verdict > 0){
            # verdict is now the index of the singularity
            # promote that element to attribute, removing names
            this <- this[[verdict]]
            names(this) <- NULL
            # restore this value to x
            attr(x[[var]], nm) <- this
          }
        }
      }
    }
  }
  x
}

#' Promote Data Frame
#'
#' Promotes a data.frame by calling \code{\link{promote.list}}.
#'
#' @param x data.frame
#' @param ... passed arguments
#' @export
#' @return same class as x
#' @keywords internal
#' @family promote
#' @examples
#' example(promote.list)
promote.data.frame <- function(x, ...)promote.list(x, ...)

#' Filter Decorated
#'
#' Filters a decorated data.frame.
#' After a filter operation, promote()
#' is called to see if ambiguous conditional
#' attributes can be improved.
#' @param .data passed to \code{\link[dplyr]{filter}}
#' @param ... passed to \code{\link[dplyr]{filter}}
#' @param .preserve passed to \code{\link[dplyr]{filter}}
#' @param .promote whether to auto-promote plural attributes
#' @importFrom dplyr filter
#' @export
#' @return decorated
#' @family promote
#' @keywords internal
#' @examples
#' library(magrittr)
#' file <- system.file(package = 'yamlet', 'extdata','phenobarb.csv')
#' x <- file %>% decorate
#'
#' # Note that there are two elements each for value label and value guide.
#' x %>% decorations(event, value)
#'
#' # Filtering promotes the relevant conditional attributes automatically.
#' x %>% filter(event == 'dose') %>% decorations(value)
#' x %>% filter(event == 'conc') %>% decorations(value)
#'
filter.decorated <- function(
  .data,
  ...,
  .preserve = FALSE,
  .promote = getOption('yamlet_promote', TRUE)
){
  y <- NextMethod()
  if(.promote) y <- promote(y)
  y <- as_decorated(y)
  y
}
#' a <- as_decorated(as.list(setNames(letters[1:3], LETTERS[1:3])))
#' attr(a$B, 'label') <- 'foo'
#' a <- a[1:3]
#' attributes(a)

#' Subset Decorated
#'
#' Subsets decorated. Calls \code{\link{promote}}
#' internally to improve ambiguous conditional
#' attributes where possible.
#'
#' @param x decorated
#' @param ... passed to next method
#' @param .promote whether to auto-promote plural attributes
#' @return decorated (unless dimension is dropped)
#' @export
#' @keywords internal
#' @family promote
#' @family decorated
#' @examples
#' library(magrittr)
#' file <- system.file(package = 'yamlet', 'extdata','phenobarb.csv')
#' x <- file %>% decorate
#' x %>% decorations(event, value)
#'
#' # Subsetting promotes automatically.
#' x[x$event == 'dose',] %>% decorations(event, value)
#' x[x$event == 'conc',] %>% decorations(event, value)
#'
#' # Dimension may be dropped
#' x[1,1]
#'
#' # Conventional subsetting
#' a <- as_decorated(as.list(setNames(letters[1:3], LETTERS[1:3])))
#' attr(a$B, 'label') <- 'foo'
#' a <- a[1:3]
#' attributes(a)
#'
`[.decorated` <- function(
  x,
  ...,
  .promote = getOption('yamlet_promote', TRUE)
){
#  stopifnot(inherits(x, 'data.frame'))
  y <- NextMethod()
  # y <- decorate(y, data.frame(x))
  # decorate(y, data.frame(x)) is
  # problematic, since decorate calls
  # decorations, which calls this method
  # decorate -> decorate.list -> as_yamlet -> decorations

  # At this point, we may have lost, rows, columns, both,
  # or been 'dropped' to one column.
  # We wish to restore column-level attributes dropped by subset.
  nms <- intersect(names(x), names(y))
  for(nm in nms){
    xat <- attributes(x[[nm]])
    if('names' %in% names(xat)){
      have <- length(xat[['names']])
      need <- length(y[[nm]])
      if (have != need){
        warning('not updating names')
        xat[['names']] <- NULL
      }
    }
    attributes(y[[nm]]) <- xat
  }
  if(.promote) y <- promote(y)
  if(is.list(y)) y <- as_decorated(y)
  y
}

