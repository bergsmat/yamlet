#' Convert Stored YAML to Intermediate Form
#'
#' Converts stored YAML to intermediate form by acquiring
#' importing the data and determining the default keys.
#'
#' @param file passed to \code{\link[yaml]{read_yaml}}
#' @param as.named.list enforced as TRUE
#' @param ... passed to \code{\link[yaml]{read_yaml}}
#' @export
#' @family yamlet
#' @return a named list
#' @examples
#' as_dyamlet(system.file('extdata','let.yaml'))
#'
as_dyamlet <- function(
  file,
  ...
){
  y <- read_yaml(file, ...) # as.named.list TRUE by default
  # result must be a list
  if(!is.list(y))stop('yamlet expects a map')
  # each member of y must be a list
  for(m in seq_along(y)) y[[m]] <- as.list(y[[m]])

  # # each element of each member of y must be a (length-one) list
  # for(m in seq_along(y)){
  #   for(e in seq_along(y[[m]])){
  #      y[[m]][[e]] <- as.list(y[[m]][[e]])
  #   }
  # }

  y[] <- lapply(y, reduce)
  y[] <- lapply(y, as.list)

  if('_keys' %in% names(y)){
    k <- y$`_keys`
    y <- y[names(y) != '_keys']
    attr(y,'keys') <- k
  }
  class(y) <- 'dyamlet'
  y
}

read_yaml('inst/extdata/let.yaml')
as_dyamlet('inst/extdata/let.yaml')

#' Convert To yamlet Format
#'
#' Converts something to yamlet format. If the object
#' or user specifies default keys, these are applied,
#' with the former having priority.  See \code{\link[as_yamlet.character]}.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @family yamlet
#' @examples
#' as.yamlet(as_dyamlet(system.file('extdata','let.yaml')))
#'
as_yamlet <- function(x, ...)UseMethod('as_yamlet')

#' Convert dyamlet To yamlet Format
#'
#' Converts something to yamlet format. If the object
#' or user specifies default keys, these are applied,
#' with the former having priority.  See \code{\link[as_yamlet.character]}.
#'
#' @param x passed to \code{\link[yaml]{read_yaml}}
#' @param default_keys character: default keys for anonymous members of each element
#' @param ... passed arguments
#' @export
#' @family yamlet
#' @return yamlet: a named list with default keys applied
#' @examples
#' as.yamlet(as_dyamlet(system.file('extdata','let.yaml')))
#'
as_yamlet.dyamlet <- function(x, default_keys = list('label','guide'), ...){
  k <- attr(x,'keys')
  if(is.null(k))k <- default_keys
  stopifnot(length(k) == length(unlist(k)))
  if(!is.character(unlist(k))){
    warning('default keys do not appear to be character: ignoring')
    k <- list()
  }
  attr(x,'keys') <- NULL
  x[] <- lapply(x, resolve, keys = k)
  class(x) <- 'yamlet'
  x
}

reduce <- function(x){ # an item (list)
  nonames1 <- is.null(names(x))
  nonames2 <- all(sapply(x, function(foo)is.null(names(foo))))
  if(nonames1 & nonames2)x[] <- lapply(x, unlist, recursive = F)
  if(nonames1 & !nonames2)x <- unlist(x, recursive = F)
  x
}


resolve <- function(x, keys){ # an item
  nms <- names(x)
  if(is.null(nms)) nms <- rep('',length(x))
  for(i in seq_along(nms)){
    if(nms[[i]] == '' & length(keys)){
      nms[[i]] <- keys[[1]]
      keys[[1]] <- NULL
    }
  }
  names(x) <- nms
  x
}

read_yaml('inst/extdata/let.yaml')
as_dyamlet('inst/extdata/let.yaml')
as_yamlet(as_dyamlet('inst/extdata/let.yaml'))


