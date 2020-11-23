#' Coerce Codelist to Factor
#'
#' Coerces codelist to factor.
#' Generic, with default and data.frame methods.
#' Returns 'classified' 'factor' which as an attribute-preserving
#' subset method.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @return class 'classified' 'factor'
#' @family factorize_codelist
factorize_codelist <- function(x,...){UseMethod('factorize_codelist')}

#' Coerce Codelist to Factor by Default
#'
#' Coerces Codelist to Factor by Default. Coerces to character and calls next method.
#'
#' @param x presumably vector-like
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @return class 'classified' 'factor'
#' @family factorize_codelist
factorize_codelist.default <- function(x,...){
  y <- as.character(x)
  attributes(y) <- attributes(x)
  factorize_codelist(y,...)
}
#' Coerce Codelist to Factor for Factor
#'
#' Coerces Codelist to Factor for Factors.
#' Coerces to character and calls next method.
#'
#' @param x factor
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @return class 'classified' 'factor'
#' @family factorize_codelist
factorize_codelist.factor <- function(x,...){
  y <- as.character(x)
  attr(x, 'levels') <- NULL
  attr(x, 'class') <- NULL
  attributes(y) <- attributes(x) # non-factor attributes
  factorize_codelist(y,...)
}

#' Coerce Character with Codelist to Factor
#'
#' Coerces character with codelist attribute to factor.
#' If attribute 'codelist' is missing, unique values of
#' x are supplied.
#'
#' @param x character
#' @param ... ignored
#' @export
#' @keywords internal
#' @return class 'classified' 'factor'
#' @family factorize_codelist
#' @examples
#' example(factorize_codelist.data.fame)
factorize_codelist.character <- function(x,...){
  guide <- attr(x,'codelist')
  if(is.null(guide)) guide <- as.list(unique(x))
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
    if(is.factor(x)){
      attributes(x) <- c(reserve, attributes(x))
      x <- as_classified(x)
    }else{
      warning('could not coerce to factor, returning character')
    }
    x
  }
}
#' Coerce Data Frame Items with Codelists to Factor
#'
#' Coerces items in data.frame with codelist attribute to factor.
#'
#' @param x data.frame
#' @param ... passed to \code{\link[dplyr]{select}} to limit scope
#' @export
#' @keywords internal
#' @return data.frame
#' @family factorize_codelist
#' @examples
#' library(magrittr)
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' x <- decorate(file)
#' x %>% explicit_guide %>% decorations(Age, Race, Heart:glyco)
#' x %>% explicit_guide %>% factorize_codelist %>% decorations(Age, Race, Heart:glyco)
#' x %>% explicit_guide %>% factorize_codelist(Heart:glyco) %>% decorations(Age, Race, Heart:glyco)

factorize_codelist.data.frame <- function(x,...){
  for(nm in selected(x,...)){
    if('codelist' %in% names(attributes(x[[nm]]))){
      x[[nm]] <- factorize_codelist(x[[nm]])
    }
  }
  x
}

