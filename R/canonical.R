#' Enforce Canonical Order
#'
#' Enforce canonical order.  Generic, with
#' method: \code{\link{canonical.decorated}}.
#' @param x object
#' @param ... passed arguments
#' @return list
#' @family canonical
#' @export
#' @keywords internal
canonical <- function(x, ...)UseMethod('canonical')

#' Sort Decorations
#'
#' Enforces canonical attribute order for class 'decorated'.
#' Set of default_keys will be augmented with all observed attribute names
#' and will be expanded or reduced as necessary for each
#' data item.
#'
#' @param x decorated
#' @param default_keys attribute names in preferred order
#' @param ... ignored
#' @export
#' @family canonical
#' @family interface
#' @return decorated
#' @examples
#' library(magrittr)
#' x <- data.frame(x = 1, y = 1, z = factor('a'))
#' x %<>% decorate('
#' x: [ guide: mm, desc: this, label: foo ]
#' "y": [ guide: bar, desc: other ]
#' ')
#'
#' decorations(x)
#' decorations(canonical(x))
#' canonical(decorations(x))
#'

canonical.decorated <- function(
  x,
  default_keys = getOption('yamlet_default_keys',list('label','guide')),
  ...
){
  for(i in seq_len(ncol(x))){
    default_keys <- union(default_keys, names(attributes(x[[i]])))
  }
  for(i in seq_len(ncol(x))){
    at <- attributes(x[[i]])
    nms <- names(at)
    use <- unlist(intersect(default_keys, nms))
    at <- at[use]
    if(length(nms)) attributes(x[[i]]) <- at
  }
  x
}

#' Sort Yamlet
#'
#' Enforces canonical attribute order for class 'yamlet'.
#' Set of default_keys will be augmented with all observed attribute names
#' and will be expanded or reduced as necessary for each
#' data item.
#'
#' @param x yamlet
#' @param default_keys attribute names in preferred order
#' @param ... ignored
#' @export
#' @keywords internal
#' @family canonical
#' @return decorated
#' @examples
#' library(magrittr)
#' x <- data.frame(x = 1, y = 1, z = factor('a'))
#' x %<>% decorate('
#' x: [ guide: mm, desc: this, label: foo ]
#' "y": [ guide: bar, desc: other ]
#' ')
#'
#' decorations(x)
#' decorations(canonical(x))
#' canonical(decorations(x))
#' write_yamlet(x)
#'

canonical.yamlet <- function(
  x,
  default_keys = getOption('yamlet_default_keys',list('label','guide')),
  ...
){
  for(i in seq_along(x)){
    default_keys <- union(default_keys, names(x[[i]]))
  }
  for(i in seq_along(x)){
    nms <- names(x[[i]])
    use <- unlist(intersect(default_keys, nms))
    if(length(nms)) x[[i]] <- x[[i]][use]
  }
  x
}









