#' Try To Look Like Something Else
#'
#' Tries to make an object look like something else.
#' Generic, with method \code{\link{mimic.default}}
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @return see methods
#' @family mimic
#' @examples
#' example(mimic.default)
mimic <- function(x, ...)UseMethod('mimic')

#' Try To Look Like Another Equal-length Variable
#'
#' Tries to mimic another vector or factor.
#' If meaningful and possible, x acquires
#' a guide attribute with labels from
#' corresponding values in y. Any codelist
#' attribute is removed. No guide is created
#' for zero-length x. If x is a factor,
#' unused levels are removed.
#'
#' @param x vector-like
#' @param y vector-like, same length as x
#' @param ... ignored arguments
#' @export
#' @importFrom stats setNames
#' @return same class as x
#' @family mimic
#' @family interface
#' @examples
#' library(magrittr)
#' library(dplyr)
#' let <- letters[1:5]
#' LET <- LETTERS[1:5]
#' int <- 0L:4L
#' num <- as.numeric(int)
#' fac <- factor(let)
#' css <- classified(let)
#'
#' # any of these can mimic any other
#' str(mimic(LET, let))
#' str(mimic(num, let))
#' str(mimic(let, num))
#' 
#' # factors get a guide and classifieds get a named codelist
#' str(mimic(fac, int))
#' str(mimic(css, int))
#' 
#' # int can 'pick up' the factor levels as guide names
#' str(mimic(int, css))
#'
#' # if two variables mean essentially the same thing,
#' # mimic lets you save space
#' x <- data.frame(id = 1:2, ID = c('A','B'))
#' x
#' x %<>% mutate(id = mimic(id, ID)) %>% select(-ID)
#' x
#' # ID still available, in principle:
#' x %>% as_decorated %>% resolve

mimic.default <- function(x, y = x, ...){
  # clear targets
  at <- attributes(x)
  nms <- names(at)
  at <- at[!nms %in% c('guide','codelist')]
  attributes(x) <- at

  # native-type levels
  z <- factor(x) # not as.factor(x), which retains unused levels if x is factor.
  ind <- match(levels(z), z)
  lev <- x[ind]
  if(is.factor(x)) lev <- as.character(lev)
  lev <- as.list(lev)

  # y-type names
  nms <- proxy(z, y)

  # reduce
  if(all(nms == unlist(lev))){
    lev <- unlist(lev)
  } else {
    lev <- setNames(lev, nms)
  }
  attr(x, 'guide') <- lev
  x
}


#' Try To Make Classified Look Like Another Equal-length Variable
#'
#' Tries to mimic another vector or factor for 'classified'.
#' See \code{\link{classified.default}}.
#' If meaningful and possible, x updates its
#' codelist attribute with labels from
#' corresponding values in y. Codes that don't occur
#' (i.e. unused levels) are removed from the codelist.
#'
#' @param x classified
#' @param y vector-like, same length as x
#' @param ... ignored arguments
#' @export
#' @keywords internal
#' @return classified
#' @family mimic
#' @examples
#' let <- letters[1:5]
#' LET <- LETTERS[1:5]
#' int <- 0L:4L
#' num <- as.numeric(int)
#' fac <- factor(let)
#' css <- classified(let)
#'
#' mimic(LET, let)
#' mimic(let, let)
#' mimic(num, let)
#' mimic(int, let)
#' mimic(fac, let)
#' mimic(css, let)
#' mimic(character(0))
#' mimic(numeric(0))
#' mimic(let, num)
#' mimic(fac, num)
#' mimic(css, num)
#' mimic(num, css)
#' mimic(let, css)
#'
#' util <- c('knife','fork','spoon')
#' util
#' factor(util)
#' classified(util)
#' mimic(util)
#' mimic(factor(util))
#' mimic(classified(util))
#'
#' x <- data.frame(let, LET)
#' library(dplyr)
#' library(magrittr)
#' x %<>% mutate(let = mimic(let, LET), LET = mimic(LET))
#' str(x)
#'
mimic.classified <- function(x, y = x, ...){
  z <- NextMethod()
  at <- attributes(z)
  nms <- names(at)
  nms[nms == 'guide'] <- 'codelist'
  names(at) <- nms
  attributes(z) <- at
  z
}

