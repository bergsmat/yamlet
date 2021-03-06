# See singularity.R for [.decorated
# Subset Decorated
#
# Subsets 'decorated', retaining attributes.
# @param x decorated
# @param ... passed to next method
# @export
# @keywords internal
# @family decorated
# @return decorated
# @examples
# a <- as_decorated(as.list(setNames(letters[1:3], LETTERS[1:3])))
# attr(a$B, 'label') <- 'foo'
# a <- a[1:3]
# attributes(a)

# `[.decorated` <- function(x, ...)as_decorated(NextMethod())

#' Assign Subset of Decorated
#'
#' Assigns subset of decorated, retaining attributes.
#' @param x decorated
#' @param ... passed to next method
#' @export
#' @keywords internal
#' @family decorated
#' @return decorated
#' @examples
#' a <- as_decorated(as.list(setNames(letters[1:3], LETTERS[1:3])))
#' a[2:3] <- 'a'
#' str(a)
#' class(a)

`[<-.decorated` <- function(x, ..., value)as_decorated(NextMethod())


#' Element-select Decorated.
#'
#' Selects element of decorated, retaining attributes.
#' @param x decorated
#' @param ... passed to next method
#' @export
#' @keywords internal
#' @family decorated
#' @return decorated
#' @examples
#' a <- as_decorated(as.list(setNames(letters[1:3], LETTERS[1:3])))
#' a[[2]]


`[[.decorated` <- function(x, ...)NextMethod()

#' Assign Element of Decorated
#'
#' Assigns element of decorated, retaining attributes.
#' @param x decorated
#' @param ... passed to next method
#' @export
#' @keywords internal
#' @family decorated
#' @return decorated
#' @examples
#' a <- as_decorated(as.list(setNames(letters[1:3], LETTERS[1:3])))
#' a[[2]]
#' a[[2]] <- 'c'
#' class(a)
#'

`[[<-.decorated` <- function(x, ..., value)as_decorated(NextMethod())

#' Assign Names of Decorated
#'
#' Assigns names of decorated, retaining attributes.
#' @param x decorated
#' @param value passed to next method
#' @export
#' @keywords internal
#' @family decorated
#' @return decorated
#' @examples
#' a <- as_decorated(as.list(setNames(letters[1:3], LETTERS[1:3])))
#' a[[2]]
#' names(a[[2]]) <- 'c'
#' class(a)
#'

`names<-.decorated` <- function(x, value)as_decorated(NextMethod())

