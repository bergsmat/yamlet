#' Unclassify Something
#'
#' Unclassify something.
#' Generic, with method \code{\link{unclassified.classified}}.
#' See also \code{\link{classified}}.
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
#'
#' Unclassifies classified.  Uses codelist attribute
#' to restore original values, preserving other attributes
#' (and rebuilding codelist).
#' @param x classified
#' @param ... ignored
#' @param persistence whether to reclass as dvec
#' @export
#' @importFrom utils type.convert
#' @keywords internal
#' @family classified
#' @return vector
#' @examples
#' example(unclassified.data.frame)
unclassified.classified <- function(x, ..., persistence = getOption('yamlet_persistence', TRUE)){
  codelist <- attr(x, 'codelist')
  levels <- unlist(codelist)
  labels <- names(codelist)
  if(is.null(labels))labels <- as.character(codelist)
  # as of 0.8.9 ...
  labels <- type.convert(labels, as.is = TRUE)
  y <- labels[match(as.character(x), levels)]
  #y <- type.convert(y, as.is = TRUE)
  nms <- names(attributes(x))
  nms <- setdiff(nms, c('class','levels','contrasts','codelist'))
  for(nm in nms){
    attr(y, nm) <- attr(x, nm)
  }
  names(labels) <- NULL # clean
  names(levels) <- NULL # clean
  codelist <- structure(as.list(labels), names = levels)
  # names codelist or unlist codelist may contain NA
  if(
    all(
      is.na(names(codelist)) == is.na(unlist(codelist)) &
      paste(names(codelist)) == paste(unlist(codelist))
    )
  ){
    names(codelist) <- NULL
    # codelist <- unlist(codelist) # @ 0.8.2 codelist remains list
  }
  attr(y, 'codelist') <- codelist
  if(persistence) y <- as_dvec(y)
  y
}

#' Unclassify Data Frame
#'
#' Unclassifies data.frame.
#' Coerces 'classified' items to original values,
#' rebuilding codelist attribute.
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

#' Unclassify Decorated Vector
#'
#' Unclassifies dvec.  A non-operation, since dvec is not classified.
#' Needed for completness by \code{\link{resolve.dvec}}.
#'
#' @param x dvec
#' @param ... ignored
#' @export
#' @keywords internal
#' @return dvec
#' @family dvec
#' @examples
#' library(magrittr)
#' x <- structure(as_dvec(1), guide = 'misc')
#' unclassified(x)
unclassified.dvec <- function(x,...)x


