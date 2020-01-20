#' Coerce Codelist to Factor
#'
#' Coerces codelist to factor.  Generic, with default and data.frame methods.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @return see methods
#' @family factorize_codelist
factorize_codelist <- function(x,...){UseMethod('factorize_codelist')}

#' Coerce Codelist to Factor by Default
#'
#' Coerces Codelist to Factor by Default. Coerces to character and calls next method.
#'
#' @param x presumably vector-like or factor
#' @param ... passed arguments
#' @export
#' @return factor
#' @family factorize_codelist
factorize_codelist.default <- function(x,...){
  # stopifnot(is.vector(x) || is.factor(x))
  y <- as.character(x)
  attributes(y) <- attributes(x)
  factorize_codelist(y,...)
}

#' Coerce Character with Codelist to Factor
#'
#' Coerces character with codelist attribute to factor.
#'
#' @param x character
#' @param ... passed arguments
#' @export
#' @return factor
#' @family factorize_codelist
#' @examples
#' example(factorize_codelist.data.fame)
factorize_codelist.character <- function(x,...){
  guide <- attr(x,'codelist')
  if(is.null(guide)){
    warning('no codelist found; returning unmodified object')
    return(x)
  }
  if(any(sapply(guide,function(i)is.null(i)))){
    warning('codelist contains NULL')
  }else{
    labs <- names(guide)
    if(is.null(labs))labs <- rep('',length(guide))
    levs <- unlist(guide)
    if(any(labs == '')){
      # warning('guide for ',item,' contains unlabeled level(s); using level itself')
      labs[labs == ''] <- levs[labs == '']
    }
    reserve <- attributes(x)
    reserve$codelist <- NULL
    try(x <- factor(x, levels = levs, labels = labs))
    if(is.factor(x)) attributes(x) <- c(reserve, attributes(x))
    x
  }
}
#' Coerce Data Frame Items with Codelists to Factor
#'
#' Coerces items in data.frame with codelist attribute to factor.
#'
#' @param x data.frame
#' @param ... passed arguments
#' @export
#' @return data.frame
#' @family factorize_codelist
#' @examples
#' library(magrittr)
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' x <- decorate(file)
#' x %>% explicit_guide %>% as_yamlet
#' x %>% explicit_guide %>% factorize_codelist %>% as_yamlet

factorize_codelist.data.frame <- function(x,...){
  for(nm in names(x)){
    if('codelist' %in% names(attributes(x[[nm]]))){
      x[[nm]] <- factorize_codelist(x[[nm]])
    }
  }
  x
}

