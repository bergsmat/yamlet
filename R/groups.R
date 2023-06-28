#' Capture Groups as Decorations
#' 
#' Captures groups as decorations.  Generic, 
#' with method \code{\link{groups_decorate.data.frame}}
#' 
#' @export
#' @keywords internal
#' @aliases NULL groups_decorate_generic
#' @family decorate
#' @param x object of dispatch
#' @param ... passed
groups_decorate <- function(x, ...)UseMethod('groups_decorate')

#' Capture Groups as Decorations for Data Frame
#' 
#' Captures groups as decorations for class 'data.frame'.
#' Creates a sequentially-valued integer attribute 
#' with name 'groups' for each corresponding column
#' (after clearing all such existing designations).
#' It is an error if not all such columns are present.
#' Defaults to \code{groups(x)}. If no columns are 
#' specified and x has no groups, x is returned
#' with any existing column-level 'groups' attributes
#' removed.
#' 
#' @export
#' @keywords internal
#' @aliases groups_decorate
#' @family decorate
#' @importFrom dplyr group_vars groups
#' @param x data.frame
#' @param ... unquoted names of columns to assign as groups; defaults to \code{groups(x)} 
#' @return same class as x
#' @examples
#' library(magrittr)
#' library(dplyr)
#' Theoph %>% groups_decorate(Subject, Time) %>% groups # nothing!
#' Theoph %>% groups_decorate(Subject, Time) %>% decorations # note well
#' Theoph %>% group_by(Subject, Time) %>% groups_decorate %>% decorations(Subject, Time) # same
#' 
groups_decorate.data.frame <- function(x, ...){
  vars <- selected(x, ..., expand = FALSE)
  if(!(length(vars))){vars <- group_vars(x)}
  stopifnot(all(vars %in% names(x)))
  x <-  modify(x, groups = NULL)
  index <- 0L
  for(var in vars){
    index <- index + 1L
    attr(x[[var]], 'groups') <- index
  }
  x
}

#' Recoover Groups Decorations
#' 
#' Recovers groups decorations.  Generic, 
#' with method \code{\link{groups_decorations.data.frame}}
#' 
#' @export
#' @keywords internal
#' @aliases NULL groups_decorations_generic
#' @family decorate
#' @param x object of dispatch
#' @param ... passed
groups_decorations <- function(x, ...)UseMethod('groups_decorations')

#' Recover Groups Decorations for Data Frame
#' 
#' Recovers groups decorations for class 'data.frame'.
#' Seeks a sequentially-valued integer attribute 
#' with name 'groups' for each column, sorts these,
#' and returns them as an un-named list of symbols,
#' like output of \code{groups(x)}.
#' 
#' @export
#' @keywords internal
#' @aliases groups_decorations
#' @family decorate
#' @importFrom dplyr group_vars groups
#' @param x data.frame
#' @param ... ignored
#' @return list of symbols
#' @examples
#' library(magrittr)
#' library(dplyr)
#' Theoph %<>% group_by(Subject, Time)
#' Theoph %>% groups_decorations # nothing!
#' Theoph %<>% groups_decorate
#' Theoph %>% groups_decorations # something!
#' Theoph %<>% ungroup
#' Theoph %>% groups # gone!
#' Theoph %<>% group_by(!!!groups_decorations(.))
#' Theoph %>% groups # recovered!
#' 
#' 
groups_decorations.data.frame <- function(x, ...){
  for(name in names(x)){
    if(is.null(attr(x[[name]], 'groups'))){
      x[[name]] <- NULL
    }
  }
  nms <- names(x)
  vals <- lapply(nms, function(nm)attr(x[[nm]], 'groups'))
  stopifnot(length(nms) == length(vals))
  for(index in seq_along(nms)){
    nm <- nms[[index]]
    val <- vals[[index]]
    val <- type.convert(val, as.is = TRUE)
    if(!(length(val) == 1))stop(nm, ': expecting length-one value  but found length ', length(val))
    if(!is.integer(val))stop(nm, ': expecting integer but found ', val)
    if(!is.finite(val))stop(nm, ': expecting finite value but found', val)
    if(!(val > 0))stop(nm, ': expecting positive value but found', val)
  }
  # all length one, safe to unlist
  if(length(vals) == 0) return(vals)
  vals <- unlist(vals)
  stopifnot(length(vals) == length(nms))
  stopifnot(is.integer(vals))
  stopifnot(all(vals > 0))
  names(vals) <- nms
  vals <- sort(vals)
  max <- max(vals)
  missing <- setdiff(seq_len(max), vals)
  if(length(missing))warning('missing indices: ', paste(missing, collapse = ', '))
  nms <- names(vals) # now sorted
  nms <- lapply(nms, sym)
  nms
}

