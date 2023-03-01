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

#' Create Classified from Factor
#' 
#' Creates classified from factor. Uses \code{\link{classified.default}},
#' but supplies existing levels by default.
#' 
#' @export
#' @return 'classified' 'factor'
#' @param x see \code{\link{factor}}
#' @param levels passed to \code{\link{classified.default}}; defaults to \code{levels(x)}
#' @param labels passed to \code{\link{classified.default}}; must be same length as levels(after removing values in \code{exclude}) and must not contain duplicates
#' @param exclude see \code{\link{factor}}
#' @param ordered see \code{\link{factor}}
#' @param nmax see \code{\link{factor}}
#' @param ... ignored
#' @importFrom dplyr distinct
#' @family classified
#' @examples
#' a <- factor(c('c','b','a'))
#' levels(classified(a))
#' attr(classified(a), 'codelist')
classified.factor <- function(
    x = character(),
    levels,
    labels,
    exclude = NA,
    ordered = is.ordered(x),
    nmax = NA,
    ...
){
  if(missing(levels)) levels <- match.fun('levels')(x)
  levels <- setdiff(levels, exclude)
  if(missing(labels)) labels <- levels
  stopifnot(identical(length(levels), length(labels)))
  if(any(duplicated(labels)))(stop('duplicated labels not supported in this context'))
  y <- classified.default(
    x,
    levels = levels,
    labels = labels,
    exclude = exclude,
    ordered = ordered,
    nmax = NA,
    ...
  )
  y
}

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
#' 
#' # classified creates a factor with a corresponding codelist attribute
#' classified(c('a','b','c'))
#' 
#' # codelist 'remembers' the origins of levels
#' classified(c('a','b','c'), labels = c('A','B','C'))
#' 
#' # classified is 'reversible'
#' library(magrittr)
#' c('a','b','c') %>%
#'   classified(labels = c('A','B','C')) %>%
#'   unclassified


classified.default <- function(
  x = character(),
  levels,
  labels,
  exclude = NA,
  ordered = is.ordered(x),
  nmax = NA,
  ...
){
  cl <- attr(x,'codelist') # could be NULL
  # if we have a codelist, use it
  if(!is.null(cl)){
    attr(x,'codelist') <- NULL

    # before working with codelist, honor the exclude request
    bad <- sapply(cl, function(val)val %in% exclude)
    cl <- cl[!bad]
    # mimic non-NA exclude behavior:
    # @ 0.10.12, commenting next (nonsensical?)
    # if(length(exclude) == 0) cl <- c(cl, NA)

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

  # under some circumstances, levels has names, which may be NA
  # then data.frame inherits NA rownames which is an error.
  names(levels) <- NULL
  names(labels) <- NULL
  codes <- data.frame(levels = levels, labels = labels)
  if(any(duplicated(codes))){
    duplicated <- anyDuplicated(codes)
    warning(
      'dropping duplicated levels, e.g.: ', 
      codes$levels[[duplicated]], 
      ' (', 
      codes$labels[[duplicated]],
      ')'
    )
    codes <- unique(codes)
  }
  
  if(any(duplicated(codes$levels))){
    duplicated <- anyDuplicated(codes$levels)
    warning(
      'level(s) cross-labelled, e.g.: ', 
      codes$levels[[duplicated]], 
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
      paste(collapse = ', ', codes$levels[codes$labels == codes$labels[[duplicated]]]), 
      ': ', 
      codes$labels[[duplicated]]
    )
  }
  
  # having dropped any duplicates, we unpack codes
  labels <- codes$labels
  levels <- codes$levels
  
  # in every case, make a good codelist
  codelist <- as.list(labels)
  names(codelist) <- levels

  # simplify codelist if possible
  if(identical(paste(names(codelist)), paste(unlist(codelist)))) {
    names(codelist) <- NULL
    # codelist <- unlist(codelist) # @v0.8.9 for consistency with other methods
  }


  # call factor()
  z <- factor(
    x = x,
    levels = levels,
    labels = labels,
    exclude = exclude, # but exclusions will have already occurred
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
  c_factor <- function (..., recursive = TRUE) { # i.e. c.factor() from R 4.1.0
    x <- list(...)
    y <- unlist(x, recursive = recursive)
    if (
      inherits(y, "factor") &&
      all(vapply(x, inherits,NA, "ordered")) &&
      (length(unique(lapply(x, levels))) == 1L)
    ) class(y) <- c("ordered", "factor")
    y
  }
  # y <- NextMethod() # not back-compatible before R 4.1.0
  y <- c_factor(..., recursive = recursive)
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

  # codelist names can be be NA but not blank
  names(codelist)[which(names(codelist) == '')] <- unlist(codelist)[which(names(codelist) == '')]
  codelist <- codelist[!duplicated(codelist)] # silently remove exact dups
  if(any(duplicated(names(codelist))))warning('conflicting codelist specifications')
  codelist <- codelist[!duplicated(names(codelist))]
  #if(all(names(codelist) == unlist(codelist))){
  if(identical(names(codelist), as.character(unlist(codelist)))){
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
#; also passed to \code{\link{classified.default}} to modify behavior
#' @param exclude see \code{\link{factor}}
#' @param ordered see \code{\link{factor}}
#' @param nmax see \code{\link{factor}}
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

classified.data.frame <- function(
    x,
    ...,
    exclude = NA,
    ordered = is.ordered(x),
    nmax = NA
){
  my_class <- class(x)
  for(nm in selected(x,...)){
    if('codelist' %in% names(attributes(x[[nm]]))){
      # grouped_df can drop subclass!
      x[[nm]] <- classified(
        x[[nm]],
        exclude = exclude,
        ordered = ordered,
        nmax = nmax
      )
    }
  }
  class(x) <- my_class
  x
}
#' Classify Decorated Vector
#'
#' Coerces dvec to 'classified':
#' a factor with a codelist attribute.
#' Results may differ if explicit_guide()
#' is called first.
#'
#' @param x dvec
#' @param ... un-named arguments ignored.  Named arguments passed to \code{\link{classified.default}} to modify behavior
#' @param exclude see \code{\link{factor}}
#' @param ordered see \code{\link{factor}}
#' @param nmax see \code{\link{factor}}
#' @export
#' @keywords internal
#' @return classified
#' @family classified
#' @family dvec
#' @examples
#' library(magrittr)
#' x <- as_dvec(1:3)
#' attr(x, 'guide') <- list(a = 1, b = 2, c = 3)
#' x %>% str
#' x %>% classified %>% str
#' x %>% explicit_guide %>% classified %>% str

classified.dvec <- function(
    x,
    ...,
    exclude = NA,
    ordered = is.ordered(x),
    nmax = NA
){
  y <- unclass(x)
  y <- classified(
    y,
    exclude = exclude,
    ordered = ordered,
    nmax = nmax,
    ...
  )
  y
}

#' Coerce Classified to Integer
#'
#' Coerces classified to integer.
#' Result is like \code{as.integer(as.numeric(x)) + offset}
#' but has a guide giving original values. If you need
#' a simple integer, consider coercing first to numeric.
#'
#' @param x classified, see \code{\link{classified}}
#' @param offset an integer value to add to intermediate result
#' @param ... passed to 
#' \code{\link{as.numeric}}, 
#' \code{\link{as.integer}}, 
#' \code{\link{desolve}}, and
#' \code{\link{mimic}}
#' @param persistence whether to return 'dvec' (is.integer(): TRUE) or just integer.
# @param exclude_attr discard these when preserving attributes of x in result
#' @export
#' @family classified
#' @return integer (possibly of class dvec)
#' @examples
#' library(magrittr)
#' 
#' # create factor with codelist attribute
#' classified(c('knife','fork','spoon'))
#' 
#' # give back a simple numeric
#' classified(c('knife','fork','spoon')) %>% as.numeric
#' 
#' # intentionally preserve levels as 'guide' attribute
#' classified(c('knife','fork','spoon')) %>% as.integer
#' 
#' # implement offset
#' classified(c('knife','fork','spoon')) %>% as.integer(-1)
#' 
#' # globally defeat the 'persistence' paradigm
#' options(yamlet_persistence = FALSE)
#' c('knife','fork','spoon') %>% 
#'   classified %>%
#'   as.integer %>% 
#'   class
#'   
#' # remove option to restore default persistence paradigm
#' options(yamlet_persistence = NULL)
#' c('knife','fork','spoon') %>% 
#'   classified %>%
#'   as.integer %>% 
#'   class
#'   
#' # locally defeat persistence paradigm
#' c('knife','fork','spoon') %>% 
#'   classified %>%
#'   as.integer(persistence = FALSE) %>% 
#'   class
#'   
#'
as.integer.classified <- function(
    x, 
    offset = 0L, 
    ..., 
    persistence = getOption('yamlet_persistence', TRUE) #,
    #exclude_attr = getOption("yamlet_as.integer_exclude_attr", c("class", "levels", "codelist"))
  ){
  stopifnot(
    length(offset) == 1,
    !is.na(offset),
    as.integer(offset) == offset
  )
  offset <- as.integer(offset)
  y <- as.numeric(x, ...)
  y <- as.integer(y, ...) # explicitly casting to int as of 0.9.0
  y <- y + offset
  z <- mimic(x, y, ...)
# r <- unclassified(z)
  r <- desolve(z, persistence = TRUE, ...) # gives guide instead of codelist at 0.9.0
  # at this point, r should be dvec
  # passing persistence to desolve fails because there is no 
  # vector method for implicit_guide (only a data.frame method)
  if(!persistence) {
    r <- unclass(r)
  }
  # for(at in names(attributes(x))){
  #   if(!at %in% exclude_attr){
  #     attr(r, at) <- attr(x, at)
  #   }
  # }
  r
}

#' Create Classified from Classified
#'
#' See \code{\link{classified.default}}.
#' Formerly (version 0.10.10), calling classified() on a
#' classified object was a non-operation.
#' Currently we call factor(x, ...) and then 
#' try to reconcile the codelist attribute with resulting 
#' levels.
#' 
#' By default classified is idempotent, such that classified(classified(x)) is
#' the same as classified(x).  In contrast, factor(factor(x)) will drop unused
#' levels (not shown). To drop unused levels, use classified(classified(x), drop = TRUE).
#'
#' @export
#' @return 'classified' 'factor'
#' @param x classified
#' @param levels passed to \code{\link{factor}}; defaults to \code{levels(x)}
#' @param labels passed to \code{\link{factor}}; must be same length as levels(after removing values in \code{exclude} and unused levels if \code{drop} is TRUE) and must not contain duplicates
#' @param exclude passed to \code{\link{factor}}
#' @param ordered passed to \code{\link{factor}}
#' @param nmax passed to \code{\link{factor}}
#' @param drop whether to drop unused levels
#' @param ... ignored
#' @keywords internal
#' @family classified
#' @examples
#' 
#' a <- 4:6
#' attr(a, 'codelist') <- list(d = 4, e = 5, f = 6, g = 7)
#' b <- classified(a)
#' a
#' b
#' class(b)
#' classified(b)
#' identical(b, classified(b))

classified.classified <- function(
    x, 
    levels,
    labels,
    exclude = NA,
    ordered = is.ordered(x),
    nmax = NA,
    drop = FALSE,
    ...
){
  if(missing(levels)) levels <- match.fun('levels')(x)
  levels <- setdiff(levels, exclude)
  if(drop) levels <- levels[levels %in% x]
  if(missing(labels)) labels <- levels
  stopifnot(identical(length(levels), length(labels)))
  if(any(duplicated(labels)))(stop('duplicated labels not supported in this context'))
  codelist <- attr(x, 'codelist')
  nms <- names(codelist) # from (character)
  vals <- as.character(unlist(codelist)) # to (coerced to character)
  stopifnot(identical(levels(x), vals)) # continuity check: should always be true
  
  y <- factor(
    x, 
    levels = levels,
    labels = labels,
    exclude = exclude,
    ordered = ordered,
    nmax = nmax
  )
  
  # now we rebuild the codelist
  # nms is the original form and order
  # levels(y) is the current from and order
  # we need a codelist with levels(y) but names from nms
  # i.e., we need to (re) map names to the current levels
  # the current levels though derive from the provided labels
  # current level order should prevail,
  # labels should be traced to provided levels,
  # and thence to provided (codelist) vals,
  # and thence to provided (codelist) nms
  
  codelist <- as.list(type.convert(levels(y), as.is = TRUE))
  # what provided values of 'levels' match existing values of 'levels', 
  # which are taken from provided 'labels'?
  was <- levels[match(levels(y), labels)]
  # now we have each former level for existing levels(y)
  # in an order corresponding to levels(y)
  # Those former levels were necessarily among the vals of former codelist.
  # we recover the meanings from nms
  meant <- nms[match(was, vals)]
  # now we know what these levels meant originally.  Possibly nothing.  Possibly NA.
  names(codelist) <- meant
  
  # all this manipulation could introduce multiple NA as codelist names.
  # in fact, codelist names should never be duplicated.
  if(any(duplicated(meant))){
    example <- meant[duplicated(meant)][[1]]
    warning('codelist names should not contain duplicates, e.g. ', example)
  }
  # enforce attributes
  nms <- names(attributes(x))
  nms <- setdiff(nms, c('class','levels','codelist','guide'))
  for(nm in nms){
    attr(y, nm) <- attr(x, nm)
  }
  
  attr(y, 'codelist') <- codelist
  class(y) <- union('classified', class(y))
  y
}

# Abbreviate Classified
# 
# Abbreviated class name for 'classified'.
# 
# @export
# @importFrom vctrs vec_ptype_abbr
# @method vec_ptype_abbr classified
# @return character
# @keywords internal
# @param x classified
# @param ... ignored
# @examples
# cat(vec_ptype_abbr(classified(0)))
# vec_ptype_abbr.classified <- function(x, ...) {
#   "clsfd"
# }

#' @importFrom pillar type_sum
#' @export
pillar::type_sum

#' Summarize Type of Classified
#' 
#' Summarizes type of classified.
#' 
#' @param x classified
#' @importFrom pillar type_sum
#' @export
#' @keywords internal
#' @method type_sum classified
#' @examples 
#' type_sum(classified(0))
type_sum.classified <- function(x){
  'clfac'
}
