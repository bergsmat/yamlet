#' Unclassify Something
#'
#' Unclassify something.
#' Generic, with method for 'classified'.
#' See \code{\link{classified}}.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family classified
#' @return see methods
#' @examples
#' example(unclassified.classified)
unclassified <- function(x, ...)UseMethod('unclassified')

#' Unclassify Classified

#' Unclassifies classified.  Uses codelist attribute
#' to restore original values, preserving other attributes
#' (and rebuilding codelist).
#' @param x classified
#' @param ... ignored
#' @export
#' @importFrom utils type.convert
#' @keywords internal
#' @family classified
#' @return vector
#' @examples
#' example(unclassified.data.frame)
unclassified.classified <- function(x, ...){
  codelist <- attr(x, 'codelist')
  levels <- unlist(codelist)
  labels <- names(codelist)
  if(is.null(labels))labels <- as.character(codelist)
  y <- levels[match(as.character(x), labels)]
  y <- type.convert(y, as.is = TRUE)
  nms <- names(attributes(x))
  nms <- setdiff(nms, c('class','levels','contrasts'))
  for(nm in nms){
    attr(y, nm) <- attr(x, nm)
  }
  codelist <- structure(as.list(labels), names = levels)
  if(all(names(codelist) == unlist(codelist)))names(codelist) <- NULL
  attr(y, 'codelist') <- codelist
  y
}

#' Unclassify Data Frame
#'
#' Unclassifies data.frame.
#' Coerces 'classified' items to original values,
#' arebuilding codelist attribute.
#'
#' @param x data.frame
#' @param ... passed to \code{\link[dplyr]{select}} to limit scope
#' @export
#' @keywords internal
#' @return data.frame
#' @family classified
#' @examples
#' library(magrittr)
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' x <- decorate(file)
#' x %>% explicit_guide %>% decorations(Age, Race, Heart:glyco)
#' x %>% explicit_guide %>% classified %>% unclassified %>% decorations(Age, Race, Heart:glyco)

unclassified.data.frame <- function(x,...){
  my_class <- class(x)
  for(nm in selected(x,...)){
    if(inherits(x[[nm]], 'classified')){
      x[[nm]] <- unclassified(x[[nm]])
    }
  }
  class(x) <- my_class
  x
}
