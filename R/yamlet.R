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

#' Retrieve Decorations
#'
#' Retrieve the decorations of something. Generic, with method for data.frame.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @family decorate
#' @return see methods
#' @examples
#' library(csv)
#' file <- system.file(package = 'yamlet', 'extdata','yamlet.csv')
#' x <- decorate(as.csv(file))
#' decorations(x)

decorations <- function(x,...)UseMethod('decorations')

#' Retrieve Decorations for Data.frame
#'
#' Retrieve the decorations of a data.frame; i.e., the metadata
#' used to decorate it. Returns a list with same names as the data.frame.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @family decorate
#' @return named list
#' @examples
#' library(csv)
#' file <- system.file(package = 'yamlet', 'extdata','yamlet.csv')
#' x <- decorate(as.csv(file))
#' decorations(x)

decorations.data.frame <- function(x,...){
  lapply(x, attributes)
}

#' Coerce Data.frame to Yamlet
#'
#' Coerces data.frame to yamlet. Assigns class 'yamlet' do a data.frame's decorations.
#'
#' @param x data.frame
#' @param... passed arguments
#' @family as_yamlet
#' @export
#' @return yamlet
#' @examples
#' library(csv)
#' file <- system.file(package = 'yamlet', 'extdata','yamlet.csv')
#' x <- decorate(as.csv(file))
#' as_yamlet(x)
as_yamlet.data.frame <- function(x, ...){
  out <- decorations(x,...)
  class(out) <- 'yamlet'
  out
}

#' Coerce yamlet to yam
#'
#' Coerces class yamlet to yam, negotiating the default keys.
#' For each member of x, names of sub-members will be dropped
#' if all previous such have been dropped.  I.e., attribute
#' order is preserved, and 'guide' (by default) will not be
#' made implicit unless 'label' has already been encountered
#' (and made implicit).  Default keys are attached as the 'keys'
#' attribute of the result.
#'
#' @param x yamlet
#' @param default_keys names that may be omitted in left subsets
#' @param ... passed arguments
#' @export
#' @return yam
#' @family yam
#' @examples
#' as_yam(as_yamlet(c('id: subject','amt: dose')))
#' as_yam(as_yamlet(c('amt: [ dose, mg ]')))
#' as_yam(as_yamlet(c('amt: [ guide: mg, label: dose ]')))
#'
as_yam.yamlet <- function(x, default_keys = list('label','guide'), ...){
  for(nm in names(x)){
    candidates <- unlist(default_keys)
    nms <- names(x[[nm]])
    for(i in seq_along(nms)){
      if(length(candidates)){
        if(identical(nms[[i]], candidates[[1]])){
          names(x[[nm]])[[i]] <- ''
          candidates <- candidates[-1]
        }
      }
    }
  }
  attr(x, 'keys') <- default_keys
  x
}

#' Coerce yam to Character
#'
#' Coerces class yam to character.  Forms the basis for a
#' yamlet emitter.
#'
#' @param x yam
#' @param ... passed arguments
#' @export
#' @family yam
#' @return character
#' @examples
#' as.character(as_yam(as_yamlet(c('id: subject','amt: dose'))))
#' as.character(
#' as_yam(
#' as_yamlet(
#' 'race: [label: race, guide: [ white: 0, black: 1, asian: 2 ], multiple: [yes: 1, no: 0]]'
#' )))
#'
as.character.yam <- function(x, ...){
    k <- attr(x, 'keys')
  if(!identical(k, list('label','guide'))){
    x <- c(x, list(`_keys` = k))
  }
  out <- paste0(names(x), ': ', sapply(x, to_yamlet))
}

#' Coerce to yamlet Storage Format
#'
#' Coerces to yamlet storage format. Generic, with methods
#' for null, character and list.
#' Always returns length-one character, possibly the empty string.
#' @param x object
#' @param ... passed arguments
#' @export
#' @return length-one character
#' @family to_yamlet
#' @examples
#' to_yamlet(as_yamlet(
#' 'race: [label: race, guide: [ white: 0, black: 1, asian: 2 ], multiple: [yes: 1, no: 0]]'
#' ))
to_yamlet <- function(x, ...)UseMethod('to_yamlet')

#' Coerce Default to yamlet Storage Format
#'
#' Coerces to yamlet storage format by default conversion to character.
#' @param x object
#' @param ... passed arguments
#' @export
#' @return length-one character
#' @family to_yamlet
#' @examples
#' to_yamlet(3)
#' to_yamlet(c(a = 4,b = 5.8))
to_yamlet.default <- function(x,...)to_yamlet(as.character(x))



#'
#' Coerce character to yamlet Storage Format
#'
#' Coerces character to yamlet storage format. Named character is processed as a named list.
#' @param x character
#' @param ... passed arguments
#' @export
#' @return length-one character
#' @family to_yamlet
#' @examples
#' to_yamlet('foo')
#' to_yamlet(c(a = 'a',b = 'b'))

to_yamlet.character <- function(x, ...){
  if(!is.null(names(x))){
    x <- as.list(x)
    return(to_yamlet(x))
  }
  # quote strings beginning with ' " [] {} > | * & ! % # ` @ ,. ? : -
  # quote yes, no, y, n
  index <- grepl("^'", x)
  x[index] <- paste0('"',x[index], '"')
  index <- grepl('^[][{}>|*&!%#`@,.?:-]', x)
  x[index] <- paste0("'",x[index],"'")
  index <- x %in% c('yes','no','y','n')
  x[index] <- paste0("'",x[index],"'")
  if(length(x) == 1) return(x)
  x <- paste(x, collapse = ', ')
  x <- paste('[', x, ']')
  x
}

#' Coerce null to yamlet Storage Format
#'
#' Coerces null to yamlet storage format (returns empty string).
#' @param x object
#' @param ... passed arguments
#' @export
#' @return length-one character
#' @family to_yamlet
#' @examples
#' to_yamlet(NULL)
to_yamlet.NULL <- function(x, ...)''

#' Coerce list to yamlet Storage Format
#'
#' Coerces list to yamlet storage format. Operates recursively on list members.
#' @param x object
#' @param ... passed arguments
#' @export
#' @return length-one character
#' @family to_yamlet
#' @examples
#' to_yamlet('foo')
#' to_yamlet(c(a = 'a',b = 'b'))
#' to_yamlet(list(a = 'a', b = 'b'))
#' meta <- system.file(package = 'yamlet', 'extdata','yamlet.yaml')
#' meta <- as_yamlet(meta)
#' to_yamlet(meta)

to_yamlet.list <- function(x, ...){
  # convert each member to yamlet
  nms <- names(x)
  out <- lapply(x, to_yamlet)
  # if member not null (''), attach name using colon-space, else using ? name
  # if name is '', do not attach
  for(i in seq_along(nms)){
    if(nms[[i]] != ''){
      if(out[[i]] != ''){
        out[[i]] <- paste(nms[[i]], out[[i]], sep = ': ')
      }else{
        out[[i]] <- paste0('? ', nms[[i]])
      }
    }
  }
  # separate members with commas
  out <- unlist(out)
  out <- paste(out, collapse = ', ')
  # enclose members in brackets
  out <- paste0('[ ', out, ' ]')
  out
}

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

#' Coerce to Axis Label
#'
#' Converts to axis label. Generic, with method \code{\link{as_lab.list}}.
#' @param x object
#' @param ... passed arguments
#' @return see methods; typically length-one character
#' @export
#' @family lab
as_lab <- function(x,...)UseMethod('as_lab')

#' Coerce List to Axis Label
#'
#' Coerces list to axis label.
#'
#' @param x list, such as returned by \code{\link{attributes}}.
#' @param default a value to return by default
#' @param collapse character: separator for collapsing multi-line units
#' @param enclose length-two character for enclosing unit
#' @param ... ignored
#' @return length-one character
#' @export
#' @family lab
#' @examples
#' meta <- system.file(package = 'yamlet', 'extdata','yamlet.csv')
#' x <- decorate(meta)
#' as_lab(attributes(x$TIME), 'TIME', enclose = c('[',']'))
as_lab.list <- function(
  x,
  default,
  collapse = '\n',
  enclose = getOption('enclose', default = c('(',')')),
  ...
){
  stopifnot(length(default) == 1, is.character(default))
  stopifnot(length(enclose) == 2, is.character(enclose))
  out <- default
  if('label' %in% names(x)) out <- x$label
  more <- character(0)
  if('units' %in% names(x)) more <- x$units
  if('unit' %in% names(x)) more <- x$unit
  if('guide' %in% names(x)){
    if(length(x$guide) == 1) more <- x$guide
  }
  if(length(more) > 1) more <- paste(more, collapse = collapse)
  if(length(more)) more <- paste0(enclose[[1]], more, enclose[[2]])
  out <- paste(out, more)
  out
}

#' Request Automatic Labels and Units for ggplot
#'
#' Requests automatic labels and units for ggplot.
#' Simply subclasses the output of ggplot, in
#' expectation of associated print method \code{\link{print.ag}}.
#'
#' @param data data.frame or similar
#' @param ... passed to \code{\link[ggplot2]{ggplot}}
#' @return return value like \code{\link[ggplot2]{ggplot}}
#' @export
#' @importFrom ggplot2 ggplot
#' @family lab
#' @examples
#' meta <- system.file(package = 'yamlet', 'extdata','yamlet.csv')
#' x <- decorate(meta)
#' library(ggplot2)
#' class(agplot(data = x) + geom_path(aes(x = TIME, y = DV)))
#' class(agplot(data = x, aes(x = TIME, y = DV)) + geom_path())

agplot <- function(data, ...){
  p <- ggplot(data = data, ...)
  class(p) <- c('ag',class(p))
  p
}
#' Print Automatic Labels and Units for ggplot
#'
#' Prints automatic labels and units for ggplot.
#' Reworks the labels as a function of attributes
#' in corresponding data. \code{labeller} will
#' receive existing labels one at a time
#' and corresponding attributes(if any) from data.
#'
#' @param x class 'ag' from \code{\link{agplot}}
#' @param labeller a function (or its name) like \code{\link{as_lab}} to generate axis labels
#' @param ... passed arguments
#' @return used for side effects
#' @export
#' @family lab
#' @examples
#' meta <- system.file(package = 'yamlet', 'extdata','yamlet.csv')
#' x <- decorate(meta)
#' library(ggplot2)
#' agplot(data = x) + geom_path(aes(x = TIME, y = DV))
#' agplot(data = x) + geom_path(aes(x = TIME, y = DV))
#' agplot(data = x, aes(x = TIME, y = DV)) + geom_path()
#' agplot(data = x) + geom_path(aes(x = TIME, y = DV)) + xlab('the time (hours)')
#' options(enclose = c('[',']'))
#' agplot(data = x) + geom_path(aes(x = TIME, y = DV))

print.ag <- function(x, labeller = getOption('labeller', default = as_lab), ...){
  fun <- match.fun(labeller)
  for(i in seq_along(x$labels)){
    lab <- x$labels[[i]]
    if(lab %in% names(x$data)){
      attr <- attributes(x$data[[lab]])
      if(!is.null(attr)){
        val <- fun(x = attr, default = lab, ...)
        x$labels[[i]] <- val
      }
    }
  }
  NextMethod()
}


