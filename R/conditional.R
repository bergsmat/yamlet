#' Test Object is Conditional
#'
#' Tests whether object is conditional.
#' @param x character
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family conditional
#' @return logical
isConditional <- function(x, ...)UseMethod('isConditional')

#' Test Object is Conditional by Default
#'
#' Tests whether object is conditional by default. Coerces to list.
#' @param x default
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family conditional
#' @return logical
#'
isConditional.default <- function(x,...)isConditional(as.list(x),...)

#' Test List is Conditional
#'
#' Tests whether a list is conditional.
#' Evaluates names of x on data and looks for meaningful result.
#' Returns TRUE if list has names and
#' all evaluate to logicals with length equal
#' to number of rows in data.
#' @param x list
#' @param data environment for variable lookup
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family conditional
#' @return length-one logical

isConditional.list <- function(x, data,...){
  nms <- names(x)
  if(!length(nms))return(FALSE)
  vals <- lapply(
    nms,
    function(i)try(
      silent = TRUE,
      eval(
        parse(text = i),
        envir = data,
        enclos = NULL
      )
    )
  )
  logi <- sapply(vals, inherits, 'logical')
  len <- sapply(vals, length)

  return(all(logi & len == nrow(data)))
}

#' Test Value is Levels
#'
#' Tests whether value is levels.
#' @param x character
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family levels
#' @return logical
isLevels <- function(x, ...)UseMethod('isLevels')

#' Test Value is Levels by Default
#'
#' Tests whether value is levels by default.  Coerces to character.
#' @param x default
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family levels
#' @return logical
isLevels.default <- function(x, table, ...)isLevels(as.character(x), table, ...)

#' Test Character Value is Levels
#'
#' Tests whether character value is levels.
#' Looks for any matches to vector.
#' Uses \code{\link{intersect}}, which is fairly flexible
#' respecting underlying data types (character 0 can match integer 0, etc.).
#' @param x default
#' @param table lookup vector
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family levels
#' @return logical

isLevels.character <- function(x, table,  ...){
  as.logical(length(intersect(x,table)) >= 1)
}


