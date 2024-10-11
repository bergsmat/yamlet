# https://vctrs.r-lib.org/reference/howto-faq-coercion.html

#' Find Common Type for classified, classified
#' 
#' Find common type for classified, classified.
#' @param x classified
#' @param y classified
#' @param ... ignored
#' @keywords internal
#' @export
#' @examples
#' library(vctrs)
#' library(magrittr)
#' c1 <- classified('a', levels = c('a','b')) %>% structure(label = 'c1')
#' c2 <- classified('b', levels = c('a','b')) %>% structure(label = 'c2')
#' c3 <- classified('c', levels = c('a','c')) %>% structure(label = 'c3')
#' f1 <- factor('a', levels = c('a','b')) %>% structure(label = 'f1')
#' f2 <- factor('b', levels = c('a','b')) %>% structure(label = 'f2')
#' f3 <- factor('c', levels = c('a','c')) %>% structure(label = 'f3')
#'
#' vec_c(c1, c1) # combined data, same codelist

#' vec_c(c1, c2) # combined data, same codelist
#' vec_c(c2, c1) # reversed data, same codelist
#' 
#' vec_c(c1, c3) # combined data, combined codelist
#' vec_c(c3, c1) # reversed data, revised codelist

#' vec_c(c1, f1) # matching levels: return classified
#' vec_c(f1, c1) # matching levels: return classified
#' 
#' vec_c(c1, f2) # matching levels: return classified
#' vec_c(f2, c1) # matching levels: return classified
#' 
#' vec_c(c1, f3) # mismatched levels: return factor
#' vec_c(f3, c1) # mismatched levels: return factor

vec_ptype2.classified.classified <- function(x, y, ...) {
  z <- c(x, y)
  z
}

#' Find Common Type for classified, factor
#' 
#' Find common type for classified, factor.
#' @param x classified
#' @param y factor
#' @param ... ignored
#' @keywords internal
#' @export
vec_ptype2.classified.factor <- function(x, y, ...){
  z <- c(x, y)
  if(!identical(levels(x), levels(y))){
    attr(z, 'codelist') <- NULL
    class(z) <- 'factor'
  }
  z
}

#' Find Common Type for factor, classified
#' 
#' Find common type for factor, classified.
#' @param x factor
#' @param y classified
#' @param ... ignored
#' @keywords internal
#' @export
vec_ptype2.factor.classified <- function(x, y, ...){
  z <- c.classified(x, y) # for symmetry with .classified.factor
  if(identical(levels(x), levels(y))){
    attr(z, 'codelist') <- attr(c(y), 'codelist') 
    # c(y) reduces codelist for symmetry with .classified.factor
    class(z) <- c('classified', 'factor')
  }
  z
}

#' Cast to classified from classified
#' 
#' Cast to classified from classified.
#' @param to classified
#' @param x classified
#' @param ... ignored
#' @keywords internal
#' @export
#' @keywords internal
#' @importFrom vctrs vec_data
vec_cast.classified.classified <- function(x, to, ...) {
  map <- match(levels(x), levels(to))
  y <- map[as.numeric(x)]
  attributes(y) <- attributes(to)
  y
}

#' Cast to classified from factor
#' 
#' Cast to classified from factor
#' @param to classified
#' @param x factor
#' @param ... ignored
#' @keywords internal
#' @export
vec_cast.classified.factor <- function(x, to, ...){
  map <- match(levels(x), levels(to))
  y <- map[as.numeric(x)]
  attributes(y) <- attributes(to)
  y
}

#' Cast to factor from classified
#' 
#' Cast to factor from classified.
#' @param to factor
#' @param x classified
#' @param ... ignored
#' @keywords internal
#' @export
vec_cast.factor.classified <- function(x, to, ...){
  map <- match(levels(x), levels(to))
  y <- map[as.numeric(x)]
  attributes(y) <- attributes(to)
  y
}
