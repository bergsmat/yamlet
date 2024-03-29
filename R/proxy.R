#' Calculate Substitute Values
#'
#' Calculates substitute values.
#' Generic, with method \code{\link{proxy.factor}}
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family proxy
#' @return see methods
#' @examples
#' example(proxy.factor)
proxy <- function(x,...)UseMethod('proxy')

#' Calculate Substitute Values for Factor Levels
#'
#' Calculates substitute values for factor levels.
#' If x and y have same length and there is a
#' one-to-one correspondence of their elements,
#' then unique elements of y are returned in
#' an order corresponding to levels(x).
#' @param x factor
#' @param y factor or vector
#' @param ... ignored
#' @export
#' @keywords internal
#' @importFrom dplyr distinct
#' @family proxy
#' @return same class as y
#' @examples
#' proxy(factor(1:3), letters[1:3])
#' proxy(factor(1:3), factor(letters[1:3]))
#' proxy(factor(letters[4:6]))
#' foo <- classified(letters[1:5])
#' as_yamlet(attributes(foo))
#' foo <- classified(foo, labels = proxy(foo))
#' as_yamlet(attributes(foo))
proxy.factor <- function(x, y = as.numeric(x), ...){
  if(length(x) != length(y))stop('x and y must have same length')
# if(!(is.factor(y)|is.vector(y)))stop('y must be vector or factor')
  if(!(is.atomic(y)))stop('y must be atomic')
  if(!(nrow(distinct(data.frame(x,y))) == length(unique(x)))){
    stop('one-to-one correspondence of x and y not detected')
  }
  vals <- y[match(levels(x), x)]
  vals
}
