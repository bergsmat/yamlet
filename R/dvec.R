#' Coerce to Decorated Vector
#'
#' Coerces to Decorated Vector. Generic, with methods
#' \code{\link{as_dvec.logical}},
#' \code{\link{as_dvec.integer}},
#' \code{\link{as_dvec.numeric}},
#' \code{\link{as_dvec.complex}}, and
#' \code{\link{as_dvec.character}}.
#' @param x object of dispatch
#' @param ... ignored arguments
#' @export
#' @keywords internal
#' @family dvec
#' @return dvec
#' @examples
#' as_dvec(0)
as_dvec <- function(x, ...)UseMethod('as_dvec')

#' Coerce Logical to Decorated Vector
#'
#' Coerces logical to decorated vector.
#' Assigns class 'dvec' and any named attributes in dots.
#' @param x logical
#' @param ... attributes to assign
#' @export
#' @keywords internal
#' @family dvec
#' @return dvec
#' @examples
#' as_dvec(c(FALSE, TRUE))
as_dvec.logical <- function(
    x,
    ...
){
  at <- list(...)
  nms <- names(at)
  nms <- nms[nms != '']
  for(nm in nms)attr(x, nm) <- at[[nm]]

  class(x) <- 'dvec'
  x
}

#' Coerce Integer to Decorated Vector
#'
#' Coerces integer to decorated vector.  Assigns class 'dvec' and any named attributes in dots.
#' @param x integer
#' @param ... attributes to assign
#' @export
#' @keywords internal
#' @family dvec
#' @return dvec
#' @examples
#' as_dvec(1:3)
as_dvec.integer <- function(x, ...){
  at <- list(...)
  nms <- names(at)
  nms <- nms[nms != '']
  for(nm in nms)attr(x, nm) <- at[[nm]]
  class(x) <- 'dvec'
  x
}

#' Coerce Numeric to Decorated Vector
#'
#' Coerces numeric to decorated vector.  Assigns class 'dvec' and any named attributes in dots.
#' @param x numeric
#' @param ... attributes to assign
#' @export
#' @keywords internal
#' @family dvec
#' @return dvec
#' @examples
#' as_dvec(c(10.3, 1.2))
#' as_dvec(1, label = 'yin')
#' as_dvec(structure(1, label = 'yin'))
#' as_dvec(structure(1, label = 'yin'), label = 'yang')
#'
as_dvec.numeric <- function(x, ...){
  at <- list(...)
  nms <- names(at)
  nms <- nms[nms != '']
  for(nm in nms)attr(x, nm) <- at[[nm]]
    class(x) <- 'dvec'
  x
}

#' Coerce Complex to Decorated Vector
#'
#' Coerces complex to decorated vector.  Assigns class 'dvec' and any named attributes in dots.
#' @param x complex
#' @param ... attributes to assign
#' @export
#' @keywords internal
#' @family dvec
#' @return dvec
#' @examples
#' as_dvec(c(complex(1), complex(2)))
as_dvec.complex <- function(x, ...){
  at <- list(...)
  nms <- names(at)
  nms <- nms[nms != '']
  for(nm in nms)attr(x, nm) <- at[[nm]]
  class(x) <- 'dvec'
  x
}

#' Coerce Character to Decorated Vector
#'
#' Coerces character to decorated vector.  Assigns class 'dvec' and any named attributes in dots.
#' @param x character
#' @param ... attributes to assign
#' @export
#' @keywords internal
#' @family dvec
#' @return dvec
#' @examples
#' as_dvec(letters)
as_dvec.character <- function(x, ...){
  at <- list(...)
  nms <- names(at)
  nms <- nms[nms != '']
  for(nm in nms)attr(x, nm) <- at[[nm]]
  class(x) <- 'dvec'
  x
}

#' Coerce Decorated Vector to Decorated Vector
#'
#' Coerces decorated vector to decorated vector.
#' Assigns any named attributes in dots.
#' @param x character
#' @param ... attributes to assign
#' @export
#' @keywords internal
#' @family dvec
#' @return dvec
#' @examples
#' as_dvec(as_dvec(letters[1:3]), label = 'Letters')
#' as_dvec(as_dvec(letters[1:3], label = 'Letters'))
as_dvec.dvec <- function(x, ...){
  at <- list(...)
  nms <- names(at)
  nms <- nms[nms != '']
  for(nm in nms)attr(x, nm) <- at[[nm]]
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
#' @keywords internal
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
#' a <- as_dvec(letters, label = 'foo')
#' a <- a[1:3]
#' attributes(a)
#' names(a) <- a
#' a[1:2]

`[.dvec` <- function(x, ...){
  y <- NextMethod()
  dropped <- setdiff(names(attributes(x)), names(attributes(y)))
  attributes(y)[dropped] <- attributes(x)[dropped]
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
#' a <- as_dvec(letters[1:3], label = 'foo')
#' a <- a[[2]]
#' attributes(a)

`[[.dvec` <- function(x, ...){
  y <- NextMethod()
  dropped <- setdiff(names(attributes(x)), names(attributes(y)))
  attributes(y)[dropped] <- attributes(x)[dropped]
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
#' a <- as_dvec(letters[1:3], label = 'foo')
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
#' a <- as_dvec(letters[1:3], label = 'foo')
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
#' a <- as_dvec(letters[1:3], label = 'foo', priority = 1)
#' b <- as_dvec(letters[3:5], label = 'foo', case = 'lower')
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

#' Reconcile Atttributes of List Members
#'
#' Reconciles attributes of list members. Recursively arbitrates
#' list members pairwise, returning the accumulated attributes.
#' @param x list
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @return list (of attributes)
#' @examples
#' library(magrittr)
#' library(dplyr)
#' a <- data.frame(study = 1) %>% decorate('study: [Study, [A: 1]]')
#' b <- data.frame(study = 2) %>% decorate('study: [Study, [B: 2]]')
#' bind_rows(a, b) %>% decorations
#' c(a$study, b$study)
#' reconcile(list(a$study, b$study))

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
      right_attr[[nm]],
      tag = nm
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
    res <- arbitrate(x, y, ...)
    if(inherits(res, 'namedList')){
      res <- unclass(res)
    }
    return(res)
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
  # bad <- duplicated(names(z)) & duplicated(z) # error for White: 1, Asian: 2, White: 2, Asian: 1.
  # z <- list(White = 1, Asian = 2, White = 2, Asian = 1)
  # z <- z[!bad]
  
  classes <- unique(sapply(z, function(i)class(i)[[1]]))
  if(length(classes) > 1)warning('mixed classes, e.g.', paste(collapse = ', ', classes[1:2]))
  
  # https://www.r-bloggers.com/2016/07/populating-data-frame-cells-with-more-than-one-value/
  codes <- data.frame(levels = I(structure(z, names = NULL)), labels = names(z))
  # TTB 0.10.22 the line above must be useful in some situations.
  # but in the simple case that each element of z is length one, 
  # unlist coerces type for better duplicate detection.
  # e.g. a mix of matching int and num in two lists won't flag as duplicates.
  # consider also: 
  # codes <- data.frame(levels = unlist(z), labels = names(z))
  
  if(any(duplicated(codes))){
    duplicated <- anyDuplicated(codes)
    # in this context, unlike classified.default, some duplication is normal
    # warning(
    #   'dropping duplicated levels, e.g.: ', 
    #   codes$levels[[duplicated]], 
    #   ' (', 
    #   codes$labels[[duplicated]],
    #   ')'
    # )
    codes <- unique(codes)
  }
  
  if(any(duplicated(codes$levels))){
    duplicated <- anyDuplicated(codes$levels)
    warning(
      'level(s) cross-labelled, e.g.: ', 
      paste( 
        collapse = ', ',
        unlist(# in case level is itself a list
          codes$levels[[duplicated]]
        )
      ), 
      ': ', 
      paste(
        collapse = ', ', 
        codes$labels[codes$levels == codes$levels[[duplicated]]]
      )
    )
  }
  if(any(duplicated(codes$labels))){
    duplicated <- anyDuplicated(codes$labels)
    warning(
      'levels like-labelled, e.g.: ', 
      paste(
        collapse = ', ', 
        codes$levels[codes$labels == codes$labels[[duplicated]]][[1]]
      ), 
      ', ',
      paste(
        collapse = ', ', 
        codes$levels[codes$labels == codes$labels[[duplicated]]][[2]]
      ), 
      ': ', 
      codes$labels[[duplicated]]
    )
  }
  
  # having dropped any duplicates, we unpack codes
  z <- as.list(codes$levels)
  names(z) <- codes$labels

  # now elements are unique, but could be like-labelled or cross-labelled.
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
arbitrate.default <- function(x, y, tag = '', ...){
  if(is.null(y)) return(x)
  # so y is not NULL.  if list, promote x and re-evaluate.
  if(is.list(y)) return(arbitrate(as.list(x), y))
  # so y is not null and not list.  Presumably vector. Hopefully scalar.
  if(identical(x, y)) return(x)
  warning(
    call. = FALSE,
    immediate. = TRUE,
    'mismatched ', tag, ' attributes: ignoring \'',
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

#' Coerce Decorated Vector to Data Frame
#'
#' Coerces decorated vector to data.frame.
#'
#' @param x dvec
#' @param row.names passed to next method
#' @param optional passed to next method
#' @param ... passed to next method
#' @param nm name for new column
#' @export
#' @keywords internal
#' @family dvec
#' @return data.frame
#' @examples
#' as.data.frame(as_dvec(letters[1:3]))
#' L <- as_dvec(letters[1:3], label = 'My Letters')
#' d <- data.frame(letters = L )
#' str(d)
as.data.frame.dvec <- function (x, row.names = NULL, optional = FALSE, ..., nm = deparse1(substitute(x)))
{
  force(nm)
  nrows <- length(x)
  if (!(is.null(row.names) || (is.character(row.names) && length(row.names) ==
                               nrows))) {
    warning(gettextf("'row.names' is not a character vector of length %d -- omitting it. Will be an error!",
                     nrows), domain = NA)
    row.names <- NULL
  }
  if (is.null(row.names)) {
    if (nrows == 0L)
      row.names <- character()
    else if (length(row.names <- names(x)) != nrows || anyDuplicated(row.names))
      row.names <- .set_row_names(nrows)
  }
  if (!is.null(names(x)))
    names(x) <- NULL
  value <- list(x)
  if (!optional)
    names(value) <- nm
  attributes(value[[1]]) <- attributes(x) # sole difference from as.data.frame.vector
  structure(value, row.names = row.names, class = "data.frame")
}

#' Coerce Decorated Vector to Units
#' 
#' Coerces dvec to units. If x has a units attribute,
#' it is used to create class 'units'. It is an error if 
#' x has no units attribute.
#' @importFrom units as_units
#' @method as_units dvec
#' @param x dvec
#' @param ... ignored
#' @param preserve attributes to preserve; just label by default (class and units are handled implicitly)
#' @export
#' @examples 
#' library(magrittr)
#' a <- data.frame(id = 1:4, wt = c(70, 80, 70, 80), sex = c(0,1,0,1))
#' a %<>% decorate('wt: [ body weight, kg ]')
#' a %<>% decorate('sex: [ sex, [ female: 0, male: 1]]')
#' a %<>% decorate('id: identifier')
#' a %<>% resolve
#' a$wt %>% as_units
as_units.dvec <- function(x, ..., preserve = getOption('yamlet_as_units_preserve', 'label')){
  value <- attr(x, 'units')
  if(is.null(value))stop('x must have non-null value of attribute: units')
  # attr(x, 'units') <- NULL
  drop <- names(attributes(x))
  drop <- setdiff(drop, preserve)
  for(nm in drop){
    attr(x, nm) <- NULL
  }
  x <- unclass(x)
  units(x) <- value
  x
}

#' @export
units::as_units

#' Coerce Units to Decorated Vector
#' 
#' Coerces units to dvec.
#' @param x units
#' @param ... passed arguments
#' @export
#' @importFrom units drop_units
#' @examples 
#' library(magrittr)
#' library(dplyr)
#' a <- data.frame(id = 1:4, wt = c(70, 80, 70, 80), sex = c(0,1,0,1))
#' a %<>% decorate('wt: [ body weight, kg ]')
#' a %<>% decorate('sex: [ sex, [ female: 0, male: 1]]')
#' a %<>% decorate('id: identifier')
#' a %<>% resolve
#' a %<>% mutate(wt = as_units(wt))
#' a %<>% mutate(wt = as_dvec(wt))
#' str(a$wt)
as_dvec.units <- function(x, ...){
  units <- deparse_unit(x)
  x <- drop_units(x)
  attr(x, 'units') <- units
  x <- as_dvec(x, ...)
  x
}

#' Abbreviate Decorated Vector
#' 
#' Abbreviated class name for dvec.
#' 
#' @export
#' @importFrom vctrs vec_ptype_abbr
#' @method vec_ptype_abbr dvec
#' @return character
#' @keywords internal
#' @param x classified
#' @param ... ignored
#' @examples
#' cat(vec_ptype_abbr(as_dvec(0)))
vec_ptype_abbr.dvec <- function(x, ...) {
  "dvec"
}

# # https://vctrs.r-lib.org/articles/s3-vector.html
#' @importFrom vctrs vec_ptype_abbr
#' @export
vctrs::vec_ptype_abbr

#' Test if Class is dvec
#' 
#' Tests whether x inherits 'dvec'.
#' @param x object
#' @export
#' @return logical
#' @examples 
#' is_dvec(1L)
#' is_dvec(as_dvec(1L))
is_dvec <- function(x){
  inherits(x, 'dvec')
}

#' Determine Scale Type for dvec
#' 
#' Determines scale type for dvec.
#' @param x dvec
#' @export
#' @keywords internal
#' @importFrom ggplot2 scale_type
#' @method scale_type dvec
scale_type.dvec <- function(x)scale_type(unclass(x))

#' Rescale dvec
#' 
#' Rescales dvec
#' @param x dvec
#' @param to numeric
#' @param from numeric
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @importFrom scales rescale
#' @method rescale dvec
rescale.dvec <- function(
    x, 
    to = c(0, 1),
    from = range(x, na.rm = TRUE, finite = TRUE), 
    ...
){
  rescale(unclass(x), to = to, from = from, ...)
}
