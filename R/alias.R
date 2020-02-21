#' Alias a Data Frame
#'
#' Aliases a data.frame.
#' Replaces column names with labels, where present.
#' Stores column name as 'alias' attribute.
#'
#' @param object data.frame
#' @param ... ignored arguments
#' @export
#' @method alias data.frame
#' @importFrom stats alias
#' @family labels
#' @return aliased data.frame
#' @examples
#' x <- data.frame(x = 1:10, y = 1:10, z = 1:10)
#' attr(x$x, 'label') <- 'Independent Value'
#' attr(x$y, 'label') <- 'Dependent Value'
#' x
#' alias(x)
alias.data.frame <- function(object, ...){
  x <- object
  for(col in names(x)){
    lab <- attr(x[[col]], 'label')
    attr(x[[col]], 'alias') <- 'col'
    if(length(lab) == 1) names(x)[match(col, names(x))] <- lab
  }
  class(x) <- union('aliased', class(x))
  x
}
