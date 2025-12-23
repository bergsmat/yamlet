#' Undecorate Something
#'
#' Undecorates something.
#' Generic, with method \code{\link{undecorate.default}}.
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family decorate
#' @return a list-like object, typically data.frame
undecorate <- function(x, ...)UseMethod('undecorate')

#' Undecorate by Default
#'
#' Undecorates by default method. Calls \code{\link{type.convert}} to 
#' each element, with \code{as.is = TRUE} by default.
#' 
#' @param x object
#' @param as.is passed to \code{\link{type.convert}}
#' @param ... passed arguments
#' @export
#' @family decorate
#' @return a list-like object, typically data.frame
#' @examples
#' file <- system.file(package = 'yamlet', 'extdata','xanomeline.csv.gz')
#' x <- io_csv(file)
#' head(decorations(x))
#' head(decorations(undecorate(x)))

undecorate.default <- function(x, as.is = TRUE, ...){
  y <- type.convert(x, as.is = TRUE, ...)
  class(y) <- setdiff(class(y), 'decorated')
  return(y)
}

#' Redecorate a List-like Object
#'
#' Redecorates a list-like object.
#' Equivalent to \code{decorate( ..., overwrite = TRUE)}.
#' If \code{meta} is not supplied, an attempt will be made
#' to redecorate with existing decorations, if any.
#'
#' @param x object
#' @param meta file path for corresponding yamlet metadata, or a yamlet object
#' @param ... passed arguments
#' @param overwrite passed to \code{\link{decorate}}
#' @export
#' @keywords internal
#' @family decorate
#' @return a list-like object, typically data.frame
#' @examples
#' library(dplyr)
#' library(magrittr)
#' library(csv)
#' library(haven)
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' x <- decorate(as.csv(file))
#' x %>% select(Subject) %>% decorations
#' x %<>% redecorate('Subject: Patient Identifier')
#' x %>% select(Subject) %>% decorations
#' 
#' # xpt may already have labels:
#' 
#' dm <- 'extdata/dm.xpt.gz' %>% 
#'   system.file(package = 'yamlet') %>% 
#'   gzfile %>% 
#'   read_xpt
#' 
#' dm %>% class  
#' dm %>% decorations(AGE, SEX, RACE)
#' 
#' # but technically not decorated, and poor persistence:
#' bind_rows(dm, dm) %>% decorations(AGE, SEX, RACE)
#' 
#' # self-redecorating helps:
#' dm %<>% redecorate
#' bind_rows(dm, dm) %>% decorations(AGE, SEX, RACE)


redecorate <- function(x, meta = NULL, ..., overwrite = TRUE){
  if(is.null(meta)){
    alt <- try(decorations(x))
    if(inherits(alt,'yamlet')) meta <- alt
  }
  decorate(x, meta = meta, ..., overwrite = overwrite)
}

#' Decorate a List-like Object
#'
#' Decorates a list-like object. Generic.
#' See \code{\link{decorate.character}}.

#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family decorate
#' @return a list-like object, typically data.frame
#' @examples
#' library(csv)
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' x <- decorate(as.csv(file))
#' identical(decorate(as.csv(file)), decorate(file))
#' decorations(x)
#'
#'
decorate <- function(x,...)UseMethod('decorate')

#' Decorate Character
#'
#' Treats \code{x} as a file path. By default,
#' metadata is sought from a file with the same
#' base but the 'yaml' extension.

#'
#' @param x file path for table data
#' @param meta file path for corresponding yamlet metadata, or a yamlet object
#' @param read function or function name for reading x
#' @param ext file extension for metadata file, if relevant
#' @param ... passed to \code{read} (if accepted) and to \code{\link{as_yamlet.character}}
#' @return class 'decorated' 'data.frame'
#' @importFrom csv as.csv
#' @export
#' @family decorate
#' @family interface
#' @examples
#' 
#' # find data file
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' file
#' 
#' # find metadata file
#' meta <- system.file(package = 'yamlet', 'extdata','quinidine.yaml')
#' meta
#' 
#' # decorate with explicit metadata reference
#' a <- decorate(file, meta)
#' 
#' # rely on default metadata path
#' b <- decorate(file)
#' 
#' # in this case: same
#' stopifnot(identical(a, b))


decorate.character <- function(
  x,
  meta = NULL,
  ...,
  read  = getOption('yamlet_import', as.csv),
  ext  = getOption('yamlet_extension', '.yaml')
  # coerce = getOption('yamlet_coerce',FALSE),
){
  stopifnot(length(x) == 1)
  if(!file.exists(x))stop('could not find file ', x)
  read <- match.fun(read)
  args <- list(...)
  # args <- args[names(args) %in% names(formals(read))] # debilitating
  args <- c(list(x),args)
  y <- do.call(read, args)
  if(is.null(meta)){
    meta <- sub('\\.[^.]*$','',x) # remove last dot and any trailing chars
    meta <- paste0(meta, ext)
  }
  if(is.character(meta) & length(meta) == 1){
    meta <- try(as_yamlet(meta,...))
  }
  if(!inherits(meta, 'yamlet')) stop('could not interpret meta: ', meta)
  decorate(
    y,
    meta = meta,
    # coerce = coerce,
    ...
  )
}

#' Decorate List
#'
#' Decorates a list-like object. Takes metadata
#' in yamlet format and loads it onto corresponding
#' list elements as attributes.
#'
#' As of v0.8.8, attribute persistence is supported 
#' by optionally coercing decorated items to class 'dvec'
#' where suitable methods exist.  \code{persistence}
#' is false by default for the list method
#' but true by default for the data.frame method.
#' See also \code{\link{decorate.data.frame}}.
#'
#' @param x object inheriting from \code{list}
#' @param meta file path for corresponding yaml metadata, or a yamlet or something coercible to yamlet; an attempt will be made to guess the file path if x has a 'source' attribute (as for \code{\link[csv]{as.csv}})
#' @param ... passed to \code{\link{as_yamlet.character}} (by method dispatch)
#' @param ext file extension for metadata file, if relevant
#' @param persistence whether to coerce decorated items to 'dvec' where suitable method exists
#' @param overwrite whether to overwrite attributes that are already present (else give warning)
#' @return like x but with 'decorated' as first class element
#' @export
#' @keywords internal
#' @family decorate
#' @examples
#' example(decorate.data.frame)
#'
decorate.list <- function(
  x,
  meta = NULL,
  ...,
  ext = getOption('yamlet_extension', '.yaml'),
  persistence = getOption('yamlet_persistence', FALSE),
  overwrite = getOption('yamlet_overwrite', FALSE)
){
  if(is.null(meta)) meta <- attr(x, 'source')
  if(is.null(meta)) stop('could not guess metadata location; supply meta')
  m <- try(silent = TRUE, as_yamlet(meta))
  if(inherits(m, 'yamlet')) meta <- m
  if(is.character(meta) & length(meta) == 1){
    meta <- sub('\\.[^.]*$','',meta) # remove last dot and any trailing chars
    meta <- paste0(meta, ext)
    meta <- try(as_yamlet(meta, ...))
  }
  if(!inherits(meta, 'yamlet')) stop('could not interpret meta: ', meta)

  for(item in names(x)){ # if list has no names, nothing happens
    if(item %in% names(meta)){ # if list has names, name '' should not be reached
      val <- meta[[item]]
      for(attrb in names(val)){
        if(attrb == ''){ # warn if name is ''
          warning('ignoring anonymous attribute for ', item)
          next
        }
        if(attrb %in% names(attributes(x[[item]]))){
          if(!overwrite){
            if(
              !identical(               # avoid moot warnings
                attr(x[[item]], attrb), # current
                val[[attrb]]            # proposed
              )
            ){
              warning('not overwriting ', attrb, ' attribute of ', item)
            }
            next                      # avoid all overwrites, moot or otherwise
          }
        }
        attr(x[[item]], attrb) <- val[[attrb]]
        # since this is really the only place we
        # assign an attribute, it is a good place
        # to coerce to dvec.  A bit redundant 
        # if more than one attribute, 
        # but safer and perhaps not too expensive.
        if(persistence){
          try(silent = TRUE, x[[item]] <- as_dvec(x[[item]]))
        }
      }
    }
  }
  # as of 0.6.2, this is the only constructor for 'decorated'
  class(x) <- union('decorated', class(x))
  x
}


#' Decorate Data Frame
#'
#' Decorates a data.frame. Expects metadata in yamlet
#' format, and loads it onto columns as attributes.
#' 
#' As of v0.8.8, the data.frame method for decorate()
#' coerces affected columns using \code{\link{as_dvec}}
#' if \code{persistence} is true and a suitable method 
#' exists.  'vctrs' methods are implemented for class 
#' \code{dvec} to help attributes persist during 
#' tidyverse operations. Details are described in
#' \code{\link{c.dvec}}. Disable this functionality
#' with \code{options(yamlet_persistence = FALSE)}.

#' @param x data.frame
#' @param meta file path for corresponding yaml metadata, or a yamlet; an attempt will be made to guess the file path if x has a 'source' attribute
#' @param ... passed to \code{\link{decorate.list}}
#' @param persistence whether to coerce decorated columns to 'dvec' where suitable method exists
#' @return class 'decorated' 'data.frame'
#' @export
#' @family interface
#' @family decorate
#' @seealso decorate.list
#' @examples
#' 
#' # find data path
#' library(csv)
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' file
#' dat <- as.csv(file) # dat now has 'source' attribute
#' 
#' # use source attribute to find metadata 
#' a <- decorate(as.csv(file))
#' 
#' # supply metadata path (or something close) explicitly
#' b <- decorate(dat, meta = file)
#' 
#' # these are equivalent
#' stopifnot(identical(a, b))

decorate.data.frame <- function(
  x,
  meta = NULL,
  ...,
  persistence = getOption('yamlet_persistence', TRUE)
)decorate.list(
  x,
  meta = meta,
  ...,
  persistence = persistence
)

#' Retrieve Decorations
#'
#' Retrieve the decorations of something.
#' Generic, with method \code{\link{decorations.data.frame}}.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family decorate
#' @return see methods
#' @examples
#' library(csv)
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' x <- decorate(as.csv(file))
#' decorations(x)

decorations <- function(x,...)UseMethod('decorations')

#' Retrieve Decorations for Data Frame
#'
#' Retrieve the decorations of a data.frame; i.e., the metadata
#' used to decorate it. Returns a list with same names as the data.frame.
#' By default, 'class' and 'level' attributes are excluded from the result,
#' as you likely don't want to manipulate these independently.

# As of 0.6.1, dropping coerce argument because of conflicts with classified().
# former help:
# Consider carefully whether the default handling of factor levels
# (see \code{coerce} argument) is appropriate for your application.

#'
#' @param x data.frame
#' @param ... optional unquoted column names to limit output (passed to \code{\link[dplyr]{select}})
# @param coerce logical: whether to coerce factor levels to guide; alternatively, a key for the levels
#' @param exclude_attr attributes to remove from the result
#' @export
#' @family decorate
#' @return named list of class 'yamlet'
#' @examples
#' # prepare a decorated data.frame
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' x <- decorate(file)
#' 
#' # retrieve the decorations
#' decorations(x, Subject, time, conc)

decorations.data.frame <- function(
  x,
  ...,
#  coerce = getOption('yamlet_coerce_decorations', FALSE),
  exclude_attr = getOption('yamlet_exclude_attr', c('class','levels'))
# and possibly comment, dim, dimnames, names, row.names, and tsp
# see help for attributes
){
  # coerce <- FALSE
  stopifnot(length(exclude_attr) == 0 || is.character(exclude_attr))
  nms <- selected(x, ...)
  x <- x[, as.character(nms), drop = FALSE] # selected may have incompatible class path
  out <- lapply(x, attributes)
  levs_key <- 'guide'
  # if(!is.logical(coerce)){
  #   if(is.character(coerce))
  #     if(length(coerce) == 1){
  #       levs_key <- coerce
  #       coerce <- TRUE
  #     }
  # }
  # if(!is.logical(coerce)){
  #   warning('coerce value not logical')
  # }else{
  #   if(coerce){
  #     for(i in seq_along(out)){
  #       if('class' %in% names(out[[i]])){
  #         if(any(out[[i]]$class == 'factor')){ # factor or ordered factor
  #           out[[i]]$class <- NULL
  #           names(out[[i]])[names(out[[i]]) == 'levels'] <- levs_key
  #         }
  #       }
  #     }
  #   }
  # }
  for(i in exclude_attr){
    for(j in names(out)){
      if(i %in% names(out[[j]])) out[[j]][[i]] <- NULL
    }
  }
  class(out) <- 'yamlet'
  out
}

# Print Decorations
#
# Prints decorations.  Coerces to yamlet and prints result.
#
# @param x decorations, i.e. a named list of class 'decorations'
# @param ... ignored
# @export
# @family decorate
# @keywords internal
# @return invisible x (yamlet)
# @examples
# example(decorations.data.frame)
# print.decorations <- function(x, ...){
#   x <- as_yamlet(x)
#   print(x)
# }

# there is no actual class 'decorations' so methods unnecessary at 0.6.2.

#' Coerce to Decorated
#'
#' Coerces to class 'decorated'. Generic, with method \code{\link{as_decorated.default}}.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @family decorate
#' @keywords internal
#' @return decorated
#' @examples
#' class(Puromycin)
#' class(as_decorated(Puromycin))
as_decorated <- function(x, ...)UseMethod('as_decorated')


#' Coerce to Decorated by Default
#'
#' Coerces to class 'decorated' by decorating (by default) with an empty list.

#'
#' @param x object
#' @param meta see \code{\link{decorate.list}}
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family decorate
#' @return decorated
#' @examples
#' class(Puromycin)
#' class(as_decorated(Puromycin))
as_decorated.default <- function(x, meta = '-', ...){
  decorate(x, meta = meta, ...)
}

# @aliases decorations.data.frame
# @keywords internal
#decorations.data.frame
