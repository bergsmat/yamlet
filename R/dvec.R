#' Coerce to Decorated Vector
#'
#' Coerces to Decorated Vector. Generic with methods
#' \code{\link{as_dvec.integer}},
#' \code{\link{as_dvec.numeric}},
#' \code{\link{as_dvec.character}}, and
#' \code{\link{as_dvec.logical}}.
#' @param x object of dispatch
#' @param ... ignored arguments
#' @export
#' @family dvec
#' @return dvec
#' @examples
#' as_dvec(0)
as_dvec <- function(x, ...)UseMethod('as_dvec')

#' Coerce Logical to Decorated Vector
#'
#' Coerces logical to decorated Vector. Expects values TRUE and/or FALSE,
#' and coerces these to character.  Assigns class 'dvec'.
#' @param x logical
#' @param ... ignored arguments
#' @export
#' @family dvec
#' @return dvec
#' @examples
#' as_dvec(FALSE:TRUE)
as_dvec.logical <- function(x, ...){
  x <- as.character(x)
  class(x) <- 'dvec'
  x
}

#' Coerce Integer to Decorated Vector
#'
#' Coerces integer to decorated Vector.  Assigns class 'dvec'.
#' @param x integer
#' @param ... ignored arguments
#' @export
#' @family dvec
#' @return dvec
#' @examples
#' as_dvec(1:3)
as_dvec.integer <- function(x, ...){
  class(x) <- 'dvec'
  x
}

#' Coerce Numeric to Decorated Vector
#'
#' Coerces numeric to decorated Vector.  Assigns class 'dvec'.
#' @param x numeric
#' @param ... ignored arguments
#' @export
#' @family dvec
#' @return dvec
#' @examples
#' as_dvec(c(10.3, 1.2))
as_dvec.numeric <- function(x, ...){
  class(x) <- 'dvec'
  x
}

#' Coerce Character to Decorated Vector
#'
#' Coerces character to decorated Vector.  Assigns class 'dvec'.
#' @param x character
#' @param ... ignored arguments
#' @export
#' @family dvec
#' @return dvec
#' @examples
#' as_dvec(letters)
as_dvec.character <- function(x, ...){
  class(x) <- 'dvec'
  x
}

# http://adv-r.had.co.nz/S3.html
# When implementing a vector class, you should implement these methods:
# length, [, [<-, [[, [[<-, c.

#' Get Length of a Decorated Vector
#'
#' Gets the length of a decorated vector.  Simply calls next method.
#'
#' @param x decorated vector
#' @export
#' @keywords inernal
#' @family dvec
#' @return integer
#' @examples
#' length(as_dvec(1:3))
length.dvec <- function(x)NextMethod()

#' Subset Decorated Vector
#'
#' Subsets decorated vector, retaining attributes.
#' @param x decorated vector
#' @param ... passed to next method
#' @export
#' @keywords internal
#' @family dvec
#' @return dvec
#' @examples
#' a <- as_dvec(letters[1:3])
#' attr(a, 'label') <- 'foo'
#' a <- a[1:3]
#' attributes(a)

`[.dvec` <- function(x, ...){
  y <- NextMethod()
  attributes(y) <- attributes(x)
  y
}

#' Element-select Decorated Vector
#'
#' Selects element of decorated vector, retaining attributes.
#' @param x decorated vector
#' @param ... passed to next method
#' @export
#' @keywords internal
#' @family dvec
#' @return dvec
#' @examples
#' a <- as_dvec(letters[1:3])
#' attr(a, 'label') <- 'foo'
#' a <- a[[2]]
#' attributes(a)

`[[.dvec` <- function(x, ...){
  y <- NextMethod()
  attributes(y) <- attributes(x)
  y
}

#' Assign Subset of Decorated Vector
#'
#' Assigns subset of decorated vector, retaining attributes.
#' @param x decorated vector
#' @param ... passed to next method
#' @export
#' @keywords internal
#' @family dvec
#' @return dvec
#' @examples
#' a <- as_dvec(letters[1:3])
#' a[2:3] <- 'a'
#' str(a)
#' class(a)

`[<-.dvec` <- function(x, ..., value){
  y <- NextMethod() # preserves attributes, including class!
  y <- unclass(y)
  class(y) <- 'dvec'
  y
}
#' Assign Element of Decorated Vector
#'
#' Assigns element of decorated vector, retaining attributes.
#' @param x decorated vector
#' @param ... passed to next method
#' @export
#' @keywords internal
#' @family dvec
#' @return dvec
#' @examples
#' a <- as_dvec(letters[1:3])
#' a[[3]] <- 'a'
#' str(a)
#' class(a)

`[[<-.dvec` <- function(x, ..., value){
  y <- NextMethod() # seems to preserve attributes, including class!
  y <- unclass(y)
  class(y) <- 'dvec'
  y
}
#' Combine Decorated Vector
#'
#' Combines decorated vectors.  Tries to preserve
#' attributes by resolving pairwise conflicts intelligently.
#' The attributes of the first item are reconciled
#' with those of the second, then those of the third, etc.
#' the \code{class} attribute is untouched.
#'
#' By default, the first version of any attribute is
#' preserved, with warning if the alternative differs.
#' NULLs are largely ignored. If either attribute
#' is a list the other is coerced to a list with as.list().
#' If either of two lists has names, then names
#' are enforced for the other (blank names if necessary).
#' Lists are combined in forward order, and
#' elements that are duplicates in value and name (if present)
#' are removed.  If the result has only blank names,
#' these are removed as well.
#'
#' @param ... items to be combined, presumably all vectors
#' @export
#' @keywords internal
#' @family dvec
#' @return dvec
#' @examples
#' a <- as_dvec(letters[1:3])
#' b <- as_dvec(letters[3:5])
#' c <- c(a,b)
#' c
#' class(c)
#'

`c.dvec` <- function( ... ){
  all <- list(...)
  for(a in all){
    if(!(is.atomic(a)))warning('expecting only atomic elements to combine')
    if(is.factor(a))stop('cannot combine dvec with factor')
  }
  y <- NextMethod()
  rec <- reconcile(all)
  rec$class <- NULL # never assign class
  attributes(y) <- rec
  class(y) <- 'dvec'
  y
}

#' Reconcile Atttributes
#'
#' Reconciles attributes.  Generic, with method \code{\link{reconcile.list}}.
#' @param x object of dispatch
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @return list (of attributes)
reconcile <- function(x, ...)UseMethod('reconcile')

#' Reconcile List Atttributes
#'
#' Reconciles list attributes. Recursively arbitrates
#' list members pairwise, returning the accumulated result.
#' @param x list
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @return list (of attributes)
reconcile.list <- function(x, ...){
  if(length(x) == 1) return(attributes(x[[1]]))
  # If we got this far, the list has length two or more.
  # Reconcile the last member with the reconciliation
  # of whatever came earlier.
  left <- x[-length(x)] # length one or more
  right <- x[[length(x)]] # just the tail
  left_attr <- reconcile(left)
  right_attr <- attributes(right)
  nms <- union(names(left_attr), names(right_attr))
  nms <- setdiff(nms, 'class') # never assign class
  best <- sapply(
    simplify = FALSE,
    USE.NAMES = TRUE,
    nms,
    function(nm)arbitrate(
      left_attr[[nm]],
      right_attr[[nm]]
    )
  )
  best
}

#' Arbitrate Two Attributes
#'
#' Arbitrates two attribute sets.  Generic, with methods
#' \code{\link{arbitrate.NULL}},
#' \code{\link{arbitrate.namedList}},
#' \code{\link{arbitrate.list}},
#' \code{\link{arbitrate.default}}.
#' @param x left attribute set
#' @param y right attribute set
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @return list (of attributes)
arbitrate <- function(x, y, ...)UseMethod('arbitrate')

#' Arbitrate Null
#'
#' Arbitrates two attributes, the first of which is NULL.
#' Simply returns the second.
#' @param x left attribute
#' @param y right attribute
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @return class of y
arbitrate.NULL <- function(x, y, ...){
  if(is.null(y))warning('unexpected NULL')
  y
}

#' Arbitrate List
#'
#' Arbitrates two attributes, the first of which is a list.
#' If x has names, it is cast as namedList and arbitrated thus.
#' @param x left attribute
#' @param y right attribute
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @return list
arbitrate.list <- function(x, y, ...){
  if(!is.null(names(x))){
    class(x) <- 'namedList'
    return(arbitrate(x, y, ...))
  }
  # if we got here, x is an un-named list
  if(is.null(y)) return(x)
  # Not sure what y is, but probably should coerce to list
  y <- as.list(y)
  # combine these two lists
  z <- c(x, y)
  # x did not have names, but if y had names, now z does also.
  # we'll put in temp names if necessary and strip them later.
  if(is.null(names(z))) names(z) <- rep('', length(z))
  # if ever both name and content are duplicated, we'll drop those.
  bad <- duplicated(names(z)) & duplicated(z)
  z <- z[!bad]
  if(all(names(z) == '')) names(z) <- NULL # probably created by us.
  z
}

#' Arbitrate Named List
#'
#' Arbitrates two attributes, the first of which is a named list.
#' @param x left attribute
#' @param y right attribute
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @return list
arbitrate.namedList <- function(x, y, ...){
  if(is.null(y)) return(x)
  # Not sure what y is, but probably should coerce to list
  y <- as.list(y)
  # combine these two lists
  z <- c(x, y)
  # since x did have names, now z does also.
  # if ever both name and content are duplicated, we'll drop those.
  bad <- duplicated(names(z)) & duplicated(z)
  z <- z[!bad]
  class(z) <- 'list'
  z
}

#' Arbitrate Default
#'
#' Arbitrates two attributes, the first of which is non-NULL and non-list.
#' If y is list, x is promoted to list and re-arbitrated thus.
#' Otherwise, x is returned, with warning if y not identical and not NULL.
#' @param x left attribute
#' @param y right attribute
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @return list if y is list, else x
arbitrate.default <- function(x, y, ...){
  if(is.null(y)) return(x)
  # so y is not NULL.  if list, promote x and re-evaluate.
  if(is.list(y)) return(arbitrate(as.list(x), y))
  # so y is not null and not list.  Presumably vector. Hopefully scalar.
  if(identical(x, y)) return(x)
  warning(
    'mismatched attributes: ignoring \'',
    paste(y, collapse = ', '),
    '\' in favor of \'',
    paste(x, collapse = ', '),
    '\''
  )
  return(x)
}

#' Format Decorated Vector
#'
#' Formats a decorated vector.
#' @param x dvec
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family dvec
#' @return character
format.dvec <- function(x, ...){
  x <- unclass(x)
  x <- NextMethod()
  x
}

#' Print Decorated Vector
#'
#' Prints a decorated vector.
#' @param x dvec
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family dvec
#' @return character
print.dvec <- function(x, ...){
  x <- unclass(x)
  x <- NextMethod()
  x
}
