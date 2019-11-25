#' Coerce to yam
#'
#' Coerce to yam, a precursor to yamlet.  Generic, with character
#' method: \code{\link{as_yam.character}}.
#' @param x object
#' @param ... passed arguments
#' @return list
#' @family yam
#' @export
as_yam <- function(x, ...)UseMethod('as_yam')

#' Coerce Character to yam
#'
#' Coerces character to yam.  Length-one character can be
#' a filepath, otherwise treated as data.  Proceeds by
#' importing the data and determining the default keys.
#'
#' @param x length-one filepath or actual data
#' @param as.named.list enforced as TRUE
#' @param ... passed to \code{\link[yaml]{read_yaml}}
#' @export
#' @importFrom yaml read_yaml yaml.load
#' @family yam
#' @return a named list
#' @examples
#' as_yam(system.file(package = 'yamlet', 'extdata','yamlet.yaml'))
#' as_yam(c('ID:','TIME:'))
#' as_yam('ID:\nTIME:')
#'
as_yam.character <- function(
  x,
  as.named.list,
  ...
){
  if(length(x) == 1 & file.exists(x[[1]])){
    y <- read_yaml(x, ...) # as.named.list TRUE by default
  }else{
    y <- try(yaml.load(paste(x, collapse = '\n')))
  }
  if(!inherits(y, 'list')){
    if(length(x) == 1){
      stop('x is not YAML or path to YAML')
    }else{
      stop('x is not YAML')
    }
  }

  # each member of y must be a list
  for(m in seq_along(y)) y[[m]] <- as.list(y[[m]])

  y[] <- lapply(y, deflate)
  y[] <- lapply(y, as.list)

  if('_keys' %in% names(y)){
    k <- y$`_keys`
    y <- y[names(y) != '_keys']
    attr(y,'keys') <- k
  }
  class(y) <- 'yam'
  y
}

#' Collapse Uninformative Levels
#'
#' Collapses uninformative levels of nested lists.
#' If any element of a list (recursively) is a list
#' with one member, that member replaces the element,
#' combining the element's name (if any) with the
#' member's name (if any).
#'
#' @param x object
#' @return list
#' @export
#' @keywords internal
#' @family deflate
#' @examples
#' deflate(yaml::yaml.load('[foo: 1, bar: 3]'))
deflate <- function(x)UseMethod('deflate')

#' Collapse Uninformative Levels by Default
#'
#' The default deflate() method returns the unmodified object.
#'
#' @param x object
#' @return list
#' @export
#' @keywords internal
#' @family deflate
#' @examples
#' deflate(yaml::yaml.load('ITEM:'))
deflate.default <- function(x)x


#' Collapse Uninformative Levels of a List
#'
#' The list method for deflate() recursively
#' ascends a nested list, removing uninformative levels.
#'
#' @param x list
#' @return list
#' @export
#' @keywords internal
#' @family deflate
#' @return list
#' @examples
#' deflate(yaml::yaml.load('ITEM: [ label: sunshine, [foo: 1, bar: 3]]'))
deflate.list <- function(x){
  # I am a list, possibly with names
  my_names <- names(x)
  if(is.null(my_names)) my_names <- rep('',length(x))
  # Now I have explicit names
  # I have child elements, possibly with names.
  # First, I collapse them.
  their_names <- lapply(x, names)
  their_lengths <- lapply(x, length)
  x[] <- lapply(x, deflate)
  # Then, for those that are length one lists,
  # I replace them with their first elements,
  # retaining the name
  for(i in seq_along(x)){
    if(their_lengths[[i]] == 1){
      x[i] <- list(x[[i]][[1]])
      my_names[[i]] <- paste0(my_names[[i]], their_names[[i]])
    }
  }
  # Now I update my own names
  names(x) <- my_names
  x
}

#' Coerce to yamlet
#'
#' Coerces something to yamlet format. If the object
#' or user specifies default keys, these are applied,
#' with the former having priority.  See \code{\link{as_yamlet.character}}.
#'
#' @param x object
#' @param ... passed arguments
#' @return a named list
#' @export
#' @family yamlet
#' @examples
#' as_yamlet(as_yam(system.file(package = 'yamlet', 'extdata','yamlet.yaml')))
#'
as_yamlet <- function(x, ...)UseMethod('as_yamlet')

#' Convert yam To yamlet Format
#'
#' Converts yam to yamlet format. If the object
#' or user specifies default keys, these are applied,
#' with the former having priority.  See \code{\link{as_yamlet.character}}.
#'
#' @param x a yam object; see \code{\link{as_yam}}
#' @param default_keys character: default keys for the first n anonymous members of each element
#' @param ... passed arguments
#' @export
#' @family yamlet
#' @return yamlet: a named list with default keys applied
#' @examples
#' as_yamlet(as_yam(system.file(package = 'yamlet', 'extdata','yamlet.yaml')))
#'
as_yamlet.yam <- function(x, default_keys = list('label','guide'), ...){
  k <- attr(x,'keys')
  if(is.null(k))k <- default_keys
  stopifnot(length(k) == length(unlist(k)))
  if(!is.character(unlist(k))){
    warning('default keys do not appear to be character: ignoring')
    k <- list()
  }
  attr(x,'keys') <- NULL
  x[] <- lapply(x, resolve, keys = k)
  class(x) <- 'yamlet'
  x
}

resolve <- function(x, keys){ # an item
  nms <- names(x)
  if(is.null(nms)) nms <- rep('',length(x))
  for(i in seq_along(nms)){
    if(nms[[i]] == '' & length(keys)){
      nms[[i]] <- keys[[1]]
      keys[[1]] <- NULL
    }
  }
  names(x) <- nms
  x
}


#' Convert Character To yamlet Format
#'
#' Converts character to yamlet format.
#' Length-one character is understood as a file path
#' if the file exists.  Otherwise, it is treated as data.
#' The file is a mapping of (nested) sequences,
#' where map keys are data item names, and
#' sequences represent data item attributes.
#' Attributes may be named or anonymous.
#' A map key '_keys' identifies a sequence of
#' key names that over-ride \code{default_keys}.
#'
#'
#' @param x length-one filepath or actual data
#' @param default_keys character: default keys for the first n anonymous members of each element
#' @param ... passed arguments
#' @export
#' @family yamlet
#' @return yamlet: a named list with default keys applied
#' @examples
#' as_yamlet(system.file(package = 'yamlet', 'extdata','yamlet.yaml'))
#' as_yamlet('ID: subject identifier')
#' as_yamlet(c('id: subject','amt: dose'))
#' as_yamlet(c('id: subject\namt: dose'))
#'
as_yamlet.character <- function(x, default_keys = list('label','guide'), ...){
  as_yamlet(as_yam(x, ...), default_keys = default_keys, ...)
}

#' Decorate a List-like Object
#'
#' Decorates a list-like object. Generic.  See \code{\link{decorate.character}}.

#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @family decorate
#' @return a list-like object, typically data.frame
#' @examples
#' library(csv)
#' file <- system.file(package = 'yamlet', 'extdata','yamlet.csv')
#' x <- decorate(as.csv(file))
#' sapply(x, attr, 'label')
#'
decorate <- function(x,...)UseMethod('decorate')

#' Decorate Character
#'
#' Treats \code{x} as a csv file path. By default,
#' metadata is sought from a file with the same
#' base but the 'yaml' extension.

#'
#' @param x file path for csv
#' @param meta file path for corresponding yaml metadata, or a yamlet
#' @param coerce whether to coerce to factor where guide is a list
#' @param ... passed arguments
#' @return data.frame
#' @importFrom csv as.csv
#' @export
#' @family decorate
#' @examples
#' file <- system.file(package = 'yamlet', 'extdata','yamlet.csv')
#' meta <- system.file(package = 'yamlet', 'extdata','yamlet.yaml')
#' x <- decorate(file)
#' x <- decorate(file, coerce = TRUE)
#' x <- decorate(file, meta = meta)
#' sapply(x, attr, 'label')
#'
decorate.character <- function(
  x,
  meta = NULL,
  coerce = FALSE,
  ...
){
  stopifnot(length(x) == 1)
  if(!file.exists(x))stop('could not find ', file)
  y <- as.csv(x,...)
  if(is.null(meta)) meta <- sub('\\.csv$','.yaml',x)
  if(is.character(meta) & length(meta) == 1){
    meta <- try(as_yamlet(meta,...))
  }
  if(class(meta) != 'yamlet') stop('could not interpret meta: ', meta)
  decorate(y, meta = meta, coerce = coerce, ... )
}

#' Decorate List
#'
#' Decorates a list-like object. Expects metadata with labels and guides,
#' where guides are units, factor levels and labels (codes, decodes), or
#' datetime formatting strings. For guides that are lists, the corresponding
#' data element may optionally be coerced to factor.

#'
#' @param x object inheriting from \code{list}
#' @param meta file path for corresponding yaml metadata, or a yamlet; an attempt will be made to guess the file path if x has a 'source' attribute
#' @param coerce whether to coerce to factor where guide is a list
#' @param ... passed arguments
#' @return list, possibly with member attributes
#' @export
#' @family decorate
#' @examples
#' example(decorate.data.frame)
#'
decorate.list <- function(
  x,
  meta = NULL,
  coerce = FALSE,
  ...
){
  if(is.null(meta)) meta <- attr(x, 'source')
  if(is.null(meta)) stop('could not guess metadata location; supply meta')
  if(is.character(meta) & length(meta) == 1){
    meta <- sub('\\.csv$','.yaml',meta)
    meta <- try(as_yamlet(meta, ...))
  }
  if(class(meta) != 'yamlet') stop('could not interpret meta: ', meta)

  for(item in names(x)){
    if(item %in% names(meta)){
      val <- meta[[item]]
      for(attrb in names(val)){
        attr(x[[item]], attrb) <- val[[attrb]]
        if(attrb == 'guide'){
          guide <- val[[attrb]]
          if(is.list(guide)){
            if(coerce){
              if(any(sapply(guide,function(i)is.null(i)))){
                warning('guide for ', item, ' contains NULL')
              }else{
                labs <- names(guide)
                levs <- unlist(guide)
                if(any(labs == '')){
                  warning('guide for ',item,' contains unlabeled levels')
                }else{
                  try(x[[item]] <- factor(x[[item]], levels = levs, labels = labs))
                }
              }
            }
          }
        }
      }
    }
  }
  x
}


#' Decorate Data Frame
#'
#' Decorates a data.frame. Expects metadata with labels and guides,
#' where guides are units, factor levels and labels (codes, decodes), or
#' datetime formatting strings. For guides that are lists, the corresponding
#' data element may optionally be coerced to factor.

#'
#' @param x data.frame
#' @param meta file path for corresponding yaml metadata, or a yamlet; an attempt will be made to guess the file path if x has a 'source' attribute
#' @param coerce whether to coerce to factor where guide is a list
#' @param ... passed arguments
#' @return data.frame
#' @export
#' @family decorate
#' @examples
#' library(csv)
#' file <- system.file(package = 'yamlet', 'extdata','yamlet.csv')
#' meta <- system.file(package = 'yamlet', 'extdata','yamlet.yaml')
#' x <- decorate(as.csv(file))
#' x <- decorate(as.csv(file))
#' x <- decorate(as.csv(file), meta = as_yamlet(meta))
#' x <- decorate(as.csv(file), meta = meta)
#' x <- decorate(as.csv(file), meta = file)
#' x <- decorate(as.csv(file), coerce = TRUE)
#' sapply(x, attr, 'label')
#'
decorate.data.frame <- function(
  x,
  meta = NULL,
  coerce = FALSE,
  ...
)decorate.list(x, meta = meta, coerce = coerce, ...)

#' Coerce List to Encoding
#'
#' Tries to coerce a list to an encoding.  Names are
#' understood as decodes, and list values as codes.
#' On failure, the list is returned unchanged.
#'
#' @param x list
#' @param ... ignored
#' @return an encoding (length-one character), or original list if error occurs
#' @export
#' @keywords internal
#' @family encode
#' @importFrom encode encoded
#' @importFrom encode encode
#' @examples
#' meta <- system.file(package = 'yamlet', 'extdata','yamlet.yaml')
#' meta <- as_yamlet(meta)
#' list2encoding(meta$ROUTE$guide)
list2encoding <- function(x, ...){
  # empty strings are effectively zero-length character
  for(i in seq_along(x)){
    if(length(x[[i]] == 1)){
      if(x[[i]] == ''){
         x[[i]] <- character(0)
      }
    }
  }
  drop <- x[lapply(x,length) == 0 ]
  if(length(drop)){
    warning(
      'dropping ',length(drop),
      ' zero-length level(s) including labels: ',
      paste(names(drop), collapse = ', ')
    )
    x <- x[lapply(x,length) != 0]
  }
  out <- unlist(x)
  nms <- names(out)
  out <- try(encode(out, labels = nms))
  if(length(out) != 1) return(x)
  if(inherits(out, 'try-error')) return(x)
  if(!encoded(out)) return(x)
  out
}

#' Encode yamelet
#'
#' Encodes yamlet.  Each 'guide' element that is a list
#' is converted to an encoding, if possible.
#'
#' @param x yamlet
#' @param target attribute to encode
#' @param ... ignored
#' @return yamlet, with guide elements possibly transformed to encodings
#' @export
#' @family encode
#' @examples
#' meta <- system.file(package = 'yamlet', 'extdata','yamlet.yaml')
#' meta <- as_yamlet(meta)
#' meta <- encode(meta)
encode.yamlet <- function(x, target = 'guide', ...){
  for(i in seq_along(x)){
    t <- x[[i]][[target]]
    if(!is.null(t)){
      if(is.list(t)){
        try <- list2encoding(t)
        if(class(try) == 'character'){
          if(length(try) == 1){
            if(encoded(try)){
              x[[i]][[target]] <- try
            }
          }
        }
      }
    }
  }
  x
}

#' @importFrom encode encode
#' @export
encode::encode

#' Subset yamelet
#'
#' Subsets yamlet. Preserves class, since a subset of yamlet is still yamlet.
#'
#' @param x object to subset
#' @param ... passed to next method
#' @return yamlet
#' @export
#' @keywords internal
#' @family encode
#' @examples
#' meta <- system.file(package = 'yamlet', 'extdata','yamlet.yaml')
#' meta <- as_yamlet(meta)
#' class(meta)
#' stopifnot(inherits(meta[1:2],'yamlet'))
`[.yamlet` <- function(x, ...){
  x <- NextMethod()
  class(x) <- 'yamlet'
  x
}

