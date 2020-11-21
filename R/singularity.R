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

#' Promote Something.
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

#' Promote by Default.
#'
#' Promotes attributes of list-like objects.
#' For the plural attributes of each element,
#' any singularity is promoted to the sole attribute.
#' Reserved attributes are untouched.
#' @param x object
#' @param ... indicated elements
#' @param .reserved attributes to leave untouched
#' @export
#' @importFrom rlang f_rhs eval_tidy quo_set_env quos
#' @return same class as x
#' @family promote
#' @family interface
#' @examples
#' file <- system.file(package = 'yamlet', 'extdata','phenobarb.csv')
#' x <- file %>% decorate
#' # Note that there are two elements each for value label and value guide.
#' x %>% decorations(time, event, value)
#' x %>% filter(event == 'dose') %>% decorations(time, event, value)
#' x %>% filter(event == 'dose') %>% promote %>% decorations(time, event, value)

promote.default <- function(
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
