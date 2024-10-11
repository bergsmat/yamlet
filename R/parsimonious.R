#' Reduce Something to its Simplest Sufficient Version
#'
#' Reduces something to its simplest sufficient version.
#' Generic, with method \code{\link{parsimonious.list}}.
#'
#' @param x object of dispatch
#' @param ... ignored
#' @keywords internal
#' @export
#' @family parsimonious
#' @examples
#' example(parsimonious.list)

parsimonious <- function(x, ...)UseMethod('parsimonious')

#' Reduce A List to its Simplest Sufficient Version
#'
#' Reduces a list to its simplest sufficient version.
#' Used internally with \code{\link[yaml]{yaml.load}}
#' as a custom handler for objects of type 'seq'.

#' Consider: \code{str(yaml.load('[a: 1, b: 2]'))}.
#' The result is technically correct. By default,
#' the parser returns a sequence of two maps.
#' Not reducible to a base type,
#' The sequence is an anonymous list.
#' The maps themselves are named lists.
#' In the special case that all elements are of length one,
#' this structure can be collapsed without semantic loss
#' to a named list.
#' More generally, if an anonymous list consists entirely
#' of length one members, those members which are
#' lists (but not already parsimonious lists)
#' can be replaced with their first elements;
#' the list becomes named if any of those elements
#' has a name. In that case, any elements without
#' names get the name '' (empty string).
#'
#' @param x object of dispatch
#' @param ... passed arguments
#' @keywords internal
#' @export
#' @family parsimonious
#' @return list
#' @examples
#' library(magrittr)
#' library(yaml)

#' # Parsimonious:
#' '[a: 1, b: 2]' %>% yaml.load
#' '[a: 1, b: 2]' %>% yaml.load(handlers = list(seq = parsimonious))
#'
#' # No effect on vector types:
#' '[1, 2]' %>% yaml.load
#' '[1, 2]' %>% yaml.load(handlers = list(seq = parsimonious))
#'
#' # Respects mixed-length vector types:
#' 'RACE: [ race, [white, black, asian ]]' %>% yaml.load
#' 'RACE: [ race, [white, black, asian ]]' %>% yaml.load(handlers = list(seq = parsimonious))
#'
#' # Anonymous elements get a blank name:
#' '[a: 1, 2]' %>% yaml.load %>% sapply(names)
#' '[a: 1, 2]' %>% yaml.load(handlers = list(seq = parsimonious)) %>% names
#'
#' # Also works for sequence of length one:
#' '[a: 1]' %>% yaml.load
#' '[a: 1]' %>% yaml.load(handlers = list(seq = parsimonious))
#'
#' # Works for NULL:
#' yaml.load('-')
#' yaml.load('-', handlers = list(seq = parsimonious))
#' 
#' # And for empty list:
#' yaml.load('[]')
#' yaml.load('[]', handlers = list(seq = parsimonious))
#'
#' # Limited to first (most deeply nested) encounter:
#' '[[[a: 1]]]' %>% yaml.load
#' '[[[a: 1]]]' %>% yaml.load(handlers = list(seq = parsimonious))
#'
#' # Works for mixed-depth nesting:
#' 'ITEM: [ label: item, [ foo: bar, hey: baz ]]' %>% yaml.load
#' 'ITEM: [ label: item, [ foo: bar, hey: baz ]]' %>% yaml.load(handlers = list(seq = parsimonious))

parsimonious.list <- function(x, ...){
  # TTB @ 1.1.3 
  # sapply below returns list() not logical for empty list.
  # trap empty list here.
  if(length(x) == 0){
    # parsimonious by definition
    class(x) <- union('parsimonious', class(x))
    return(x)
  }
  
  # are any of these lists parsimonious?
  parsimonious <- sapply(x, inherits, 'parsimonious')

  # are any members longer than one element?
  plural <- sapply(x, length) > 1

  # are any non-parsimonious members plural?
  extensive <- any(!parsimonious & plural)

  # is this list anonymous?
  # anonymous <- is.null(names(x))

  # do we have any lists?
  isList <- sapply(x, is.list)

  # do we have any NULL?
  isNull <- sapply(x, is.null)

  # targets are non-parsimonious lists
  targets <- isList & !parsimonious

  # unconditionally unclass parsimonious lists
  x[parsimonious] <- lapply(x[parsimonious], unclass)

  # reduce if any targets, and not extensive
  if(!extensive & any(targets)){
    y <- list()
    for(i in seq_along(x)){
      if(targets[[i]]) {
        y[[i]] <- x[[i]]
      } else {
        y[[i]] <- x[i]
      }
    }
    x <- do.call(c, y)
  }

  # if there were no lists or null, then convert to vector
  # 2022-02-21 @v8.2, this simplification seems not to support
  # length-one un-named codelists, such as 'sex: [ Sex, [M] ]'
  # if(!any(isList) & !any(isNull) & !any(parsimonious)) x <- unlist(x)

  class(x) <- union('parsimonious', class(x))
  x
}

#' Reduce by Default to Simplest Sufficient Version
#'
#' Reduces by default to simplest sufficient version.
#' This is a companion to \code{\link{parsimonious.list}}
#' and currently returns an unmodified object.
#'
#' @param x object for which no specific parsimonious method exists
#' @param ... ignored
#' @export
#' @keywords internal
#' @family parsimonious
parsimonious.default <- function(x, ...){
  x
}

