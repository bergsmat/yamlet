#' Alias a Data Frame
#'
#' Aliases a data.frame.
#' Replaces column names with labels, where present.
#' Stores column name as 'alias' attribute.
#'
#' @param object data.frame
#' @param ... optional unquoted names of target columns
#' @export
#' @keywords internal
#' @method alias data.frame
#' @importFrom stats alias
#' @family labels
#' @family deprecated
#' @return aliased data.frame
#' @examples
#' library(magrittr)
#' d <- data.frame(x = 1:10, y = 1:10, z = 1:10)
#' d %<>% modify(x, label = 'Independent Value')
#' d %<>% modify(y, label = 'Dependent Value')
#' d
#' alias(d)
#' alias(d, y)
alias.data.frame <- function(object, ...){
  x <- object
  y <- selected(x, ...)
  for(col in y){
    lab <- attr(x[[col]], 'label')
    attr(x[[col]], 'alias') <- col
    if(length(lab) == 1) names(x)[match(col, names(x))] <- lab
  }
  class(x) <- union('aliased', class(x))
  x
}
