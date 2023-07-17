#' Capture Groups as Decorations
#' 
#' Captures groups as decorations.  Generic, 
#' with method \code{\link{decorate_groups.data.frame}}
#' 
#' @export
#' @keywords internal
#' @aliases NULL decorate_groups_generic
#' @family decorate
#' @param x object of dispatch
#' @param ... passed
decorate_groups <- function(x, ...)UseMethod('decorate_groups')

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
#' @aliases  decorate_groups
#' @family decorate
#' @importFrom dplyr group_vars groups
#' @param x data.frame
#' @param ... unquoted names of columns to assign as groups; defaults to \code{groups(x)} 
#' @return same class as x
#' @examples
#' library(magrittr)
#' library(dplyr)
#' Theoph %>% decorate_groups(Subject, Time) %>% groups # nothing!
#' Theoph %>% decorate_groups(Subject, Time) %>% decorations # note well
#' Theoph %>% group_by(Subject, Time) %>% decorate_groups %>% decorations # same
#' 
decorate_groups.data.frame <- function(x, ...){
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

#' Recover Groups Decorations
#' 
#' Recovers groups decorations.  Generic, 
#' with method \code{\link{decorations_groups.data.frame}}
#' 
#' @export
#' @keywords internal
#' @aliases NULL decorations_groups_generic
#' @family decorate
#' @param x object of dispatch
#' @param ... passed
decorations_groups <- function(x, ...)UseMethod('decorations_groups')

#' Recover Groups Decorations for Data Frame
#' 
#' Recovers groups decorations for class 'data.frame'.
#' Seeks a sequentially-valued integer attribute 
#' with name 'groups' for each column, sorts these,
#' and returns a character vector like \code{group_vars(x)}.
#' 
#' @export
#' @aliases  decorations_groups
#' @family decorate
#' @importFrom dplyr group_vars groups
#' @param x data.frame
#' @param ... ignored
#' @return character: names of groups columns
#' @examples
#' library(magrittr)
#' library(dplyr)
#' Theoph %<>% group_by(Subject, Time)
#' Theoph %>% group_vars
#' Theoph %>% decorations_groups # nothing!
#' Theoph %<>% decorate_groups
#' Theoph %>% decorations_groups # something!
#' Theoph %<>% ungroup
#' Theoph %>% group_vars # gone!
#' Theoph %<>% group_by(across(all_of(decorations_groups(.))))
#' Theoph %>% group_vars # recovered!
#' Theoph %<>% group_by_decorations
#' Theoph %>% group_vars # same
#' rm(Theoph)
#' 
decorations_groups.data.frame <- function(x, ...){
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
  if(length(vals) == 0) return(as.character(vals))
  # all length one, safe to unlist
  vals <- unlist(vals)
  stopifnot(length(vals) == length(nms))
  stopifnot(is.integer(vals))
  stopifnot(all(vals > 0))
  names(vals) <- nms
  vals <- sort(vals)
  max <- max(vals)
  missing <- setdiff(seq_len(max), vals)
  if(length(missing))warning('missing indices: ', paste(missing, collapse = ', '))
  dup <- anyDuplicated(vals)
  if(dup)warning('duplicated indices, e.g. ', vals[[dup]], ': ', names(vals)[[dup]])
  nms <- names(vals) # now sorted
  #nms <- lapply(nms, sym)
  nms
}

#' Group by Decorations
#' 
#' Groups according to decorations.  Generic, 
#' with method \code{\link{group_by_decorations.data.frame}}
#' 
#' @export
#' @keywords internal
#' @aliases NULL group_by_decorations_generic
#' @family decorate
#' @param x object of dispatch
#' @param ... passed
group_by_decorations <- function(x, ...)UseMethod('group_by_decorations')

#' Groups by Decorations for Data Frame
#' 
#' Invokes \code{\link[dplyr]{group_by}}
#' using whatever groups are recovered by
#' \code{\link{decorations_groups}}.
#' 
#' @export
#' @aliases  group_by_decorations
#' @family decorate
#' @importFrom dplyr group_by across all_of
#' @importFrom rlang sym
#' @param x grouped_df
#' @param ... ignored
#' @return list of symbols
#' @examples
#' library(magrittr)
#' library(dplyr)
#' Theoph %>% group_vars # nothing!
#' Theoph %<>% decorate_groups(Subject, Time) 
#' Theoph %<>% group_by_decorations
#' Theoph %>% group_vars # something
#' rm(Theoph)
#' 
group_by_decorations.data.frame <- function(x, ...){
  gr <- decorations_groups(x)
  x <- group_by(x, across(all_of(gr)))
  x
}

