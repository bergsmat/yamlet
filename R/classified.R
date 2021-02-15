#' Classify Something
#'
#' Classifies something.
#' Generic, with method \code{\link{classified.default}}
#' @param x object of dispatch
#' @param ... passed arguments
#' @export
#' @return see methods
#' @keywords internal
#' @family classified
#' @examples
#' example(classified.default)
classified <- function(x, ...)UseMethod('classified')


#' Create Classified by Default
#'
#' Creates a factor of subclass 'classified',
#' for which there are attribute-preserving methods.
#' In particular, classified has a codelist attribute
#' indicating the origin of its levels: it is
#' constructed from the codelist attribute of x
#' if available, or from 'levels' and 'labels'
#' by default. Unlike the case for \code{\link{factor}},
#' length of labels cannot be one (i.e., different from
#' length of levels).
#'
#' @export
#' @return 'classified' 'factor'
#' @param x see \code{\link{factor}}
#' @param levels see \code{\link{factor}}
#' @param labels see \code{\link{factor}}, must have same length as levels
#' @param exclude see \code{\link{factor}}
#' @param ordered see \code{\link{factor}}
#' @param nmax see \code{\link{factor}}
#' @param ... ignored
#' @importFrom dplyr distinct
#' @family classified
#' @examples
#' classified(1:3)
#' classified(1:3, levels = 4:6)
#' classified(1:3, levels = 1:3)
#' classified(1:3, labels = letters[1:3])

classified.default <- function(
  x = character(),
  levels,
  labels = levels,
  exclude = NA,
  ordered = is.ordered(x),
  nmax = NA,
  ...
){
  cl <- attr(x,'codelist')
  # if we have a codelist, use it
  if(!is.null(cl)){
    attr(x,'codelist') <- NULL

    # before working with codelist, honor the exclude request
    bad <- sapply(cl, function(val)val %in% exclude)
    cl <- cl[!bad]

    # default levels and labels
    if(missing(levels)){
      levels <- unlist(cl)
    }
    if(missing(labels)){
      labels <- names(cl)
      if(is.null(labels))labels <- rep('', length(levels))
      labels[labels == ''] <- levels[labels == '']
    }
  }

  # if no codelist, set up default labels and levels
  if (missing(levels)) {
    y <- unique(x, nmax = nmax)
    ind <- order(y)
    levels <- unique(as.character(y)[ind])
    levels <- setdiff(levels, exclude)
  }
  if(missing(labels)){
    labels <- as.character(levels)
  }

  # at this point, levels and labels should have matching length
  # should be true using defaults
  if(length(levels) != length(labels))stop('classified requires labels and levels of the same length')

  # in every case, make a good codelist
  codelist <- as.list(labels)
  names(codelist) <- levels

  # simplify codelist if possible
  if(identical(paste(names(codelist)), paste(unlist(codelist)))) {
    names(codelist) <- NULL
    codelist <- unlist(codelist)
  }


  # call factor()
  z <- factor(
    x = x,
    levels = levels,
    labels = labels,
    exclude = exclude,
    ordered = ordered,
    nmax = nmax
  )

  # enforce attributes
  nms <- names(attributes(x))
  nms <- setdiff(nms, c('class','levels'))
  for(nm in nms){
    attr(z, nm) <- attr(x, nm)
  }
  attr(z, 'codelist') <- codelist

  # enforce class
  class(z) <- union('classified', class(z))

  # return
  z
}

# Coerce to Classified
#
# Coerce something to classified.
# Generic, with method for factor.
# Deprecated.  Prefer classified().
#
# @param x object
# @param ... passed arguments
# @export
# @keywords internal
# @family classified
# @return see methods
# @examples
# example(as_classified.factor)
# as_classified <- function(x, ...)UseMethod('as_classified')

# Coerce Factor to Classified
#
# Coerce factor to classified.
# Creates a factor that retains attributes during subsetting.
# Deprecated.  Prefer classified().
#
# @param x factor
# @param ... ignored arguments
# @export
# @keywords internal
# @family classified
# @return class 'classified' 'factor'
# @examples
# class(as_classified(factor(letters)))
# as_classified.factor <- function(x, ...){
#   class(x) <- union('classified', class(x))
#   x
# }


# http://adv-r.had.co.nz/S3.html
# When implementing a vector class, you should implement these methods:
#length, [, [<-, [[, [[<-, c.


#' Subset Classified
#'
#' Subsets classified factor, retaining attributes.
#' @param x classified factor
#' @param ... passed to next method
#' @export
#' @keywords internal
#' @family classified
#' @return class 'classified' 'factor'
#' @examples
#' a <- classified(letters[1:3])
#' attr(a, 'label') <- 'foo'
#' a <- a[1:3]
#' attributes(a)

`[.classified` <- function(x, ...){
  y <- NextMethod()
  # contrasts and levels will have been handled
  nms <- names(attributes(x))
  nms <- setdiff(nms, c('contrasts','levels'))
  for(nm in nms){
    attr(y, nm) <- attr(x, nm)
  }
  y
}

#' Element-select Classified
#'
#' Selects element of classified factor, retaining attributes.
#' @param x classified factor
#' @param ... passed to next method
#' @export
#' @keywords internal
#' @family classified
#' @return class 'classified' 'factor'
#' @examples
#' a <- classified(letters[1:3])
#' attr(a, 'label') <- 'foo'
#' a <- a[[2]]
#' attributes(a)

`[[.classified` <- function(x, ...){
  y <- NextMethod()
  # contrasts and levels will have been handled
  nms <- names(attributes(x))
  nms <- setdiff(nms, c('contrasts','levels'))
  for(nm in nms){
    attr(y, nm) <- attr(x, nm)
  }
  y
}

#' Assign Subset of Classified
#'
#' Assigns subset of classified factor, retaining attributes.
#' @param x classified factor
#' @param ... passed to next method
#' @export
#' @keywords internal
#' @family classified
#' @return class 'classified' 'factor'
#' @examples
#' a <- classified(letters[1:3])
#' a[2:3] <- 'a'
#' str(a)
#' class(a)

`[<-.classified` <- function(x, ..., value){
  y <- NextMethod()
  # class and levels will have been handled
  nms <- names(attributes(x))
  nms <- setdiff(nms, c('levels')) # implicitly restore class
  for(nm in nms){
    attr(y, nm) <- attr(x, nm)
  }
  y
}
#' Assign Element of Classified
#'
#' Assigns element of classified factor, retaining attributes.
#' @param x classified factor
#' @param ... passed to next method
#' @export
#' @keywords internal
#' @family classified
#' @return class 'classified' 'factor'
#' @examples
#' a <- classified(letters[1:3])
#' a[[3]] <- 'a'
#' str(a)
#' class(a)

`[[<-.classified` <- function(x, ..., value){
  y <- NextMethod()
  # class and levels will have been handled
  nms <- names(attributes(x))
  nms <- setdiff(nms, c('levels')) # implicitly restore class
  for(nm in nms){
    attr(y, nm) <- attr(x, nm)
  }
  y
}
#' Combine Classified
#'
#' Combines classified factor, retaining attributes.
#' Attributes other than levels and codelist are taken
#' from the first argument.  Attribute 'levels' is
#' supplied by next method.  Attribute 'codelist'
#' is the combined codelists in sequence of
#' all (dots) arguments, after silently removing
#' exact duplicates, and then removing
#' duplicated names with warning.
#'
#' @param x classified factor
#' @param ... passed to next method
#' @export
#' @keywords internal
#' @family classified
#' @return class 'classified' 'factor'
#' @examples
#' a <- classified(letters[1:3])
#' b <- classified(letters[3:5])
#' c <- c(a,b)
#' c
#' class(c)
#'

`c.classified` <- function( ..., recursive = TRUE ){
  y <- NextMethod()
  # class and levels will have been handled
  all <- list(...)
  x <- all[[1]]
  nms <- names(attributes(x))
  nms <- setdiff(nms, c('levels')) # implicitly restore class
  for(nm in nms){
    attr(y, nm) <- attr(x, nm)
  }
  # combine levels
  codelist <- list()
  for(i in 1:length(all)){
    codelist <- c(codelist, attr(all[[i]], 'codelist'))
  }
  # explicit names
  if(is.null(names(codelist)))names(codelist) <- unlist(codelist)
  names(codelist)[names(codelist) == ''] <- unlist(codelist)[names(codelist) == '']
  codelist <- codelist[!duplicated(codelist)] # silently remove exact dups
  if(any(duplicated(names(codelist))))warning('conflicting codelist specifications')
  codelist <- codelist[!duplicated(names(codelist))]
  if(all(names(codelist) == unlist(codelist))){
    names(codelist) <- NULL
    codelist <- unlist(codelist)
  }
  attr(y,'codelist') <- codelist
  y
}

#' Classify Data Frame
#'
#' Coerces items in data.frame with codelist attribute to 'classified':
#' a factor with a codelist attribute.
#'
#' @param x data.frame
#' @param ... passed to \code{\link[dplyr]{select}} to limit column scope
#' @export
#' @keywords internal
#' @return data.frame
#' @family classified
#' @family interface
#' @examples
#' library(magrittr)
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' x <- decorate(file)
#' x %>% explicit_guide %>% decorations(Age, Race, Heart:glyco)
#' x %>% explicit_guide %>% classified %>% decorations(Age, Race, Heart:glyco)
#' x %>% explicit_guide %>% classified(Heart:glyco) %>% decorations(Age, Race, Heart:glyco)

classified.data.frame <- function(x,...){
  my_class <- class(x)
  for(nm in selected(x,...)){
    if('codelist' %in% names(attributes(x[[nm]]))){
      x[[nm]] <- classified(x[[nm]]) # grouped_df can drop subclass!
    }
  }
  class(x) <- my_class
  x
}

