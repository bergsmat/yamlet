#' Redecorate a List-like Object
#'
#' Redecorates a list-like object.
#' Equivalent to \code{decorate( ..., overwrite = TRUE)}.

#'
#' @param x object
#' @param ... passed arguments
#' @param overwrite passed to \code{\link{decorate}}
#' @export
#' @family decorate
#' @return a list-like object, typically data.frame
#' @examples
#' library(dplyr)
#' library(magrittr)
#' library(csv)
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' x <- decorate(as.csv(file))
#' x %>% select(Subject) %>% as_yamlet
#' x %<>% redecorate('Subject: Patient Identifier')
#' x %>% select(Subject) %>% as_yamlet
#'
redecorate <- function(x, ..., overwrite = TRUE)decorate(x, ..., overwrite = overwrite)

#' Decorate a List-like Object
#'
#' Decorates a list-like object. Generic.
#' See \code{\link{decorate.character}}.

#'
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
#' decorations(x) # but prefer as_yamlet(x)
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
#' @param ... passed to read (if accepted) and to \code{\link{as_yamlet.character}}
#' @return class 'decorated' 'data.frame'
#' @importFrom csv as.csv
#' @export
#' @family decorate
#' @family interface
#' @examples
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' meta <- system.file(package = 'yamlet', 'extdata','quinidine.yaml')
#' identical(
#'   decorate(file),
#'   decorate(file, meta)
#' )
#' identical(
#'   decorate(file, meta = as_yamlet(meta)),
#'   decorate(file, meta = meta)
#' )
#' a <- decorate(file)
#' b <- resolve(decorate(file))
#' c <- resolve(decorate(
#'   file,
#'   read = read.table,
#'   quote = "",
#'   as.is = FALSE,
#'   sep = ',',
#'   header = TRUE,
#'   na.strings = c('', '\\s', '.','NA'),
#'   strip.white = TRUE,
#'   check.names = FALSE
#' ))
#' d <- decorate(
#'   file,
#'   read = read.table,
#'   quote = "",
#'   as.is = FALSE,
#'   sep = ',',
#'   header = TRUE,
#'   na.strings = c('', '\\s', '.','NA'),
#'   strip.white = TRUE,
#'   check.names = FALSE
#' )
#'
#' # Importantly, b and c are identical with respect to factors
#' cbind(
#'   `as.is/!resolve`   = sapply(a, class), # no factors
#'   `as.is/resolve`    = sapply(b, class), # factors made during decoration
#'   `!as.is/resolve`   = sapply(c, class), # factors made twice!
#'   `!as.is/!resolve`  = sapply(d, class)  # factors made during read
#' )
#' str(a$Smoke)
#' str(b$Smoke)
#' str(c$Smoke)
#' str(d$Smoke)
#' levels(c$Creatinine)
#' levels(d$Creatinine) # level detail retained as 'guide'

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
  if(class(meta) != 'yamlet') stop('could not interpret meta: ', meta)
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
#' list elements as attributes. This is the only
#' function that creates class 'decorated'.
#'
#'
#' @param x object inheriting from \code{list}
#' @param meta file path for corresponding yaml metadata, or a yamlet or something coercible to yamlet; an attempt will be made to guess the file path if x has a 'source' attribute (as for \code{\link[csv]{as.csv}})
#' @param ... passed to \code{\link{as_yamlet.character}} (by method dispatch)
#' @param ext file extension for metadata file, if relevant
# @param coerce whether to coerce to factor where guide has length > 1
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
  #coerce = getOption('yamlet_coerce', FALSE),
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
  if(class(meta) != 'yamlet') stop('could not interpret meta: ', meta)

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
            warning('not overwriting ', attrb, ' attribute of ', item)
            next
          }
        }
        attr(x[[item]], attrb) <- val[[attrb]]
      }
    }
  }
  class(x) <- union('decorated', class(x))
  x
}


#' Decorate Data Frame
#'
#' Decorates a data.frame. Expects metadata in yamlet
#' format, and loads it onto columns as attributes.

#'
#' @param x data.frame
#' @param meta file path for corresponding yaml metadata, or a yamlet; an attempt will be made to guess the file path if x has a 'source' attribute
#' @param ... passed to \code{\link{decorate.list}}
# @param coerce whether to coerce to factor where guide is a list
#' @return class 'decorated' 'data.frame'
#' @export
#' @family interface
#' @family decorate
#' @seealso decorate.list
#' @examples
#' library(csv)
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' meta <- system.file(package = 'yamlet', 'extdata','quinidine.yaml')
#' a <- decorate(as.csv(file))
#' b <- decorate(as.csv(file), meta = as_yamlet(meta))
#' c <- decorate(as.csv(file), meta = meta)
#' d <- decorate(as.csv(file), meta = file)
#' e <- resolve(decorate(as.csv(file)))
#'
#' # Most import methods are equivalent.
#' identical(a, b)
#' identical(a, c)
#' identical(a, d)
#' identical(a, e)
decorate.data.frame <- function(
  x,
  meta = NULL,
  ...
  # coerce = getOption('yamlet_coerce',FALSE),
)decorate.list(
  x,
  meta = meta,
  ...
)

#' Retrieve Decorations
#'
#' Retrieve the decorations of something.
#' Generic, with method for data.frame.
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
#' decorations(x) # prefer as_yamlet(x)

decorations <- function(x,...)UseMethod('decorations')

#' Retrieve Decorations for Data Frame
#'
#' Retrieve the decorations of a data.frame; i.e., the metadata
#' used to decorate it. Returns a list with same names as the data.frame.
#' By default, \code{class} attributes are excluded from the result,
#' as this is an attribute you likely don't want to manipulate independently.

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
#' @return named list of class 'decorations'
#' @examples
#' library(csv)
#' library(magrittr)
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' x <- decorate(as.csv(file))[,c('conc','Race')]
#' y <- decorate(as.csv(file))[,c('conc','Race')] %>% resolve
#' decorations(x)
#' decorations(y)
#' decorations(y, conc)
#' decorations(y, exclude_attr = NULL)

decorations.data.frame <- function(
  x,
  ...,
#  coerce = getOption('yamlet_coerce_decorations', FALSE),
  exclude_attr = getOption('yamlet_exclude_attr', c('class','levels'))
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

#' Print Decorations
#'
#' Prints decorations.  Coerces to yamlet and prints result.
#'
#' @param x decorations, i.e. a named list of class 'decorations'
#' @param ... ignored
#' @export
#' @family decorate
#' @return invisible x (yamlet)
#' @examples
#' example(decorations.data.frame)
print.decorations <- function(x, ...){
  x <- as_yamlet(x)
  print(x)
}

#' Coerce to Decorated
#'
#' Coerces to class 'decorated'. Generic, with default method.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @family decorate
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
#' @family decorate
#' @return decorated
#' @examples
#' class(Puromycin)
#' class(as_decorated(Puromycin))
as_decorated.default <- function(x, meta = '-', ...){
  decorate(x, meta = meta, ...)
}
