#' Coerce to Yam
#'
#' Coerce to yam, a precursor to yamlet.  Generic, with character
#' method: \code{\link{as_yam.character}}.
#' @param x object
#' @param ... passed arguments
#' @return list
#' @family yam
#' @export
#' @keywords internal
as_yam <- function(x, ...)UseMethod('as_yam')

#' Coerce Character to Yam
#'
#' Coerces character to yam.  Length-one character can be
#' a filepath, otherwise treated as data.  Proceeds by
#' importing the data and determining the default keys.
#'
#' @param x length-one filepath or actual data
#' @param as.named.list enforced as TRUE
#' @param ... passed to \code{\link[yaml]{read_yaml}} and \code{\link[yaml]{yaml.load}} if supported
#' @export
#' @keywords internal
#' @importFrom yaml read_yaml yaml.load
#' @family yam
#' @keywords internal
#' @return a named list
#' @examples
#'
#' # Read sample data from file.
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.yaml')
#' file
#' as_yam(file)
#'
#' # Read yamlet directly from character vector.
#' as_yam(c('ID:','TIME:'))
#'
#' # Read from length-one character (same result).
#' as_yam('ID:\nTIME:')
#'
as_yam.character <- function(
  x,
  as.named.list,
  handlers = list(seq = parsimonious, map = function(x)lapply(x, unclass)),
  ...
){
  if(length(x) == 1 & file.exists(x[[1]])){
    allowed <- c(names(formals(read_yaml)), names(formals(yaml.load)))
    args <- list(...)
    args <- args[names(args) %in% allowed ]
    args <- c(
      list(
        file = x,
        as.named.list = TRUE,
        handlers = handlers
      ),
      args
    )
    y <- do.call(read_yaml, args)
  }else{
    allowed <- c(names(formals(yaml.load)))
    args <- list(...)
    args <- args[names(args) %in% allowed ]
    dat <- paste(x, collapse = '\n')
    args <- c(
      list(
        string = dat,
        handlers = handlers,
        as.named.list = TRUE
      ),
      args
    )
    y <- do.call(yaml.load, args)
  }
  # should just be a bare list
  y <- unclass(y)

  # its members should be bare lists
  y[] <- lapply(y, unclass)

  if(!inherits(y, 'list')){
    if(length(x) == 1){
      stop('x is not YAML or path to YAML')
    }else{
      stop('x is not YAML')
    }
  }

  # ? each member of y must be a list
  # for(m in seq_along(y)) y[[m]] <- as.list(y[[m]])

  # un-nesting is now applied at parsing using 'parsimonious'
  # y[] <- lapply(y, unnest)

  # but list coercion still important ...
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
#' Each element of a list that is itself a list
#' and does not have a name but has exactly one element
#' that DOES have a name should become that element
#' and have that name (recursively, from depth).
#' Collapses uninformative levels of nested lists.
#' Formerly used in as_yam; now superceded by \code{\link{parsimonious.list}}.
#'
#' @param x object
#' @return named list
#' @export
#' @keywords internal
#' @family unnest
#' @examples
#'
#' # yaml.load reads this as a list of two un-named lists whose elements are named.
#' str(yaml::yaml.load('[foo: 1, bar: 3]'))
#'
#' # yamlet treats it as a list of two named integers.
#' str(unnest(yaml::yaml.load('[foo: 1, bar: 3]')))
unnest <- function(x,...)UseMethod('unnest')


#' Collapse Uninformative Levels by Default
#'
#' The default unnest() method returns the unmodified object.
#'
#' @param x object
#' @return list
#' @export
#' @keywords internal
#' @family unnest
#' @examples
#' unnest(yaml::yaml.load('ITEM:'))
unnest.default <- function(x, ...)x


#' Collapse Uninformative Levels of a List
#'
#' The list method for unnest() recursively
#' ascends a nested list, removing uninformative levels.
#'
#' @param x list
#' @export
#' @keywords internal
#' @family unnest
#' @return list
#' @examples
#'
#' a <- 'ITEM: [ label: sunshine, [foo: 1, bar: 3]]'
#'
#' #  yaml.load() sees label nested one-deep, and foo nested two-deep:
#' yaml::yaml.load(a)
#'
#' # unnest() sees label nested zero-deep, and foo nested one-deep:
#' unnest(yaml::yaml.load(a))
#'
#' # as_yamlet() provides explicit name (default key) for second element of ITEM:
#' as_yamlet(a)

unnest.list <- function(x,...){
  # make names explicit
  if(is.null(names(x))) names(x) <- rep('',length(x))
  # unnest members
  x[] <- lapply(x, unnest) # for atomics
  # if I reached here, I am a list
  # and all my members are unnested.
  # process each element:
  for(i in seq_along(x)){
    this <- x[[i]]                    # element i
    nm <- names(x)[[i]]               # name of element i
    len <- length(this)               # length one?
    isList <- is.list(this)           # list?
    nms <- setdiff(names(this),'')    # good names
    if(
      isList &                        # each element that is a list
      nm == '' &                      # and does not have a name
      len == 1 &                      # but has exactly one element
      length(nms)                     # that is named
    ){
      x <- append(x, this, after = i)[-i] # should be that element and have that name
    }
  }
  x
}

#' Coerce to Yamlet
#'
#' Coerces something to yamlet format. If the object
#' or user specifies default keys, these are applied.
#' See \code{\link{as_yamlet.character}}.
#'
#' @param x object
#' @param ... passed arguments
#' @return a named list
#' @export
#' @keywords internal
#' @family yamlet
#' @examples
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.yaml')
#' file
#' identical(as_yamlet(as_yam(file)), as_yamlet(file))
#'
#' # Read yamlet from storage and apply default keys.
#' as_yamlet(file)
#'
as_yamlet <- function(x, ...)UseMethod('as_yamlet')

#' Coerce Yam To Yamlet Format
#'
#' Coerces yam to yamlet format. If the object
#' or user specifies default keys, these are applied
#' See \code{\link{as_yamlet.character}}.
#'
#' @param x a yam object; see \code{\link{as_yam}}
#' @param default_keys character: default keys for the first n anonymous members of each element
#' @param ... ignored
#' @export
#' @family yamlet
#' @keywords internal
#' @return yamlet: a named list with default keys applied
#' @examples
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.yaml')
#' file
#' as_yamlet(as_yam(file))
#'
as_yamlet.yam <- function(x, default_keys = getOption('yamlet_default_keys',list('label','guide')), ...){
  default_keys <- as.list(default_keys)
  k <- attr(x,'keys')
  if(is.null(k))k <- as.list(default_keys)
  stopifnot(length(k) == length(unlist(k)))
  if(!is.character(unlist(k))){
    warning('default keys do not appear to be character: ignoring')
    k <- list()
  }
  attr(x,'keys') <- NULL
  x[] <- lapply(x, .resolve, keys = k)
  unresolved <- which(sapply(x, function(i)any(names(i) == '')))
  if(length(unresolved))warning('missing key(s) for element(s) ', paste(unresolved, collapse = ', '))
  class(x) <- 'yamlet'
  x
}

.resolve <- function(x, keys){ # an item
  nms <- names(x)
  if(is.null(nms)) nms <- rep('',length(x))
  for(i in seq_along(nms)){
    if(length(keys)){ # if we have unused defaults
      if(nms[[i]] == ''){
        nms[[i]] <- keys[[1]]
        keys[[1]] <- NULL
      }else{
        keys <- setdiff(keys, nms[[i]]) # if a default is given, it is used
      }
    }
  }
  names(x) <- nms
  x
}


#' Coerce Character To Yamlet Format
#'
#' Coerces character to yamlet format.
#' Length-one character is understood as a file path
#' if the file exists.  Otherwise, it is treated as data.
#' The file is a mapping of (nested) sequences,
#' where map keys are data item names, and
#' sequences represent data item attributes.
#' Attributes may be named or anonymous.
#'
#' If an attribute is anonymous, an attempt
#' is made to name it using available defaults.
#' A special item named '_keys' if present identifies a sequence of
#' key names that over-ride \code{default_keys}.
#' Attribute names are sought first in the explicit yaml,
#' then in the special item named '_keys',
#' then in the \code{default_keys} argument passed to \code{\link{as_yamlet}},
#' then in \code{options()$yamlet_default_keys},
#' then in the defaults for argument \code{default_keys}.
#'
#'
#' @param x length-one filepath or actual data
#' @param default_keys character: default keys for the first n anonymous members of each element
#' @param ... passed to \code{\link{as_yam.character}} and \code{\link{as_yamlet.yam}}
#' @export
#' @keywords internal
#' @family yamlet
#' @return yamlet: a named list with default keys applied
#' @examples
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.yaml')
#' as_yamlet(file)
#' as_yamlet('ID: subject identifier')
#' as_yamlet(c('id: subject','amt: dose'))
#' as_yamlet(c('id: subject\namt: dose'))
#'
as_yamlet.character <- function(x, default_keys = getOption('yamlet_default_keys', list('label','guide')), ...){
  as_yamlet(as_yam(x, ...), default_keys = default_keys, ...)
}


#' Coerce Data Frame to Yamlet
#'
#' Coerces data.frame to yamlet by calling {\code{\link{decorations.data.frame}}}.
#'
#' @param x data.frame
#' @param ... passed to \code{\link{decorations}}
#' @family yamlet
#' @export
#' @keywords internal
#' @return yamlet
#' @examples
#' library(csv)
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' x <- decorate(as.csv(file))
#' as_yamlet(x)
as_yamlet.data.frame <- function(x, ...){
  out <- decorations(x,...)
  # class(out) <- 'yamlet' # as of 0.6.2 decorations() returns yamlet
  out
}
#' Coerce List to Yamlet
#'
#' Coerces list to yamlet. Assigns class 'yamlet'.
#' Checks that list has names.
#'
#' @param x list
#' @param ... ignored
#' @family yamlet
#' @export
#' @keywords internal
#' @return yamlet
#' @examples
#' x <- list(a = 2, b = 3)
#' as_yamlet(x)
as_yamlet.list <- function(x, ...){
  if(is.null(names(x)))stop('list must have names for conversion to yamlet')
  class(x) <- 'yamlet'
  x
}
#' Coerce Yamlet to Yamlet
#'
#' Coerces yamlet to yamlet. Currently a non-operation.
#'
#' @param x yamlet
#' @param ... ignored
#' @family yamlet
#' @export
#' @keywords internal
#' @return yamlet
#' @examples
#' meta <- system.file(package = 'yamlet', 'extdata','quinidine.yaml')
#' x <- as_yamlet(meta)
#' as_yamlet(x)
as_yamlet.yamlet<- function(x, ...)x

#' Coerce Yamlet to Yam
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
#' @param ... ignored
#' @param default_keys names that may be omitted in left subsets
#' @export
#' @keywords internal
#' @return yam
#' @family yam
#' @keywords internal
#' @examples
#' as_yam(as_yamlet(c('id: subject','amt: dose')))
#' as_yam(as_yamlet(c('amt: [ dose, mg ]')))
#' as_yam(as_yamlet(c('amt: [ guide: mg, label: dose ]')))
#'
as_yam.yamlet <- function(
  x,
  ...,
  default_keys = getOption('yamlet_default_keys', list('label','guide'))
){
  default_keys <- as.list(default_keys)
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
  class(x) <- 'yam'
  x
}

#' Coerce Yam to Character
#'
#' Coerces class yam to character.  Forms the basis for a
#' yamlet emitter.
#'
#' @param x yam
#' @param ... ignored; keys is an attribute of yam
#' @export
#' @keywords internal
#' @family yam
#' @keywords internal
#' @return character
#' @examples
#' foo <- as_yamlet(c('id: subject','amt: dose'))
#' class(foo)
#' bar <- as_yam(foo)
#' class(bar)
#' as.character(bar)
#' as.character(
#' as_yam(
#' as_yamlet(
#' "race: [label: race, guide: [ white: 0, black: 1, asian: 2 ], multiple: ['yes': 1, 'no': 0]]"
#' )))
#'
as.character.yam <- function(x, ...){
    k <- attr(x, 'keys')
  if(!identical(k, list('label','guide'))){
    x <- c(x, list(`_keys` = k))
  }
  out <- paste0(names(x), ': ', sapply(x, to_yamlet))
}

#' Coerce to Yamlet Storage Format
#'
#' Coerces to yamlet storage format. Generic, with methods
#' for default, null, character and list which together
#' implement the yamlet storage syntax.
#' Always returns length-one character, possibly the empty string.
#'
#' @param x object
#' @param ... ignored
#' @export
#' @keywords internal
#' @return length-one character
#' @family to_yamlet
to_yamlet <- function(x, ...)UseMethod('to_yamlet')

#' Coerce Default to Yamlet Storage Format
#'
#' Coerces to yamlet storage format by default conversion to character.
#' @param x object
#' @param ... ignored
#' @export
#' @keywords internal
#' @return length-one character
#' @family to_yamlet
#' @examples
#' to_yamlet(3)
#' to_yamlet(c(a = '4',b = '5.8'))
#' to_yamlet(c(a = 4,b = 5.8))
#' to_yamlet(TRUE)

to_yamlet.default <- function(x,...)to_yamlet(sapply(x, as.character))

#' Coerce Yamlet to Yamlet Storage Format
#'
#' Coerces yamlet to yamlet storage format by unclassing to list.
#' @param x object
#' @param ... ignored
#' @export
#' @keywords internal
#' @return length-one character
#' @family to_yamlet
#' @examples
#' library(magrittr)
#'  'a: [[d: [0, 1, 2]]]' %>% as_yamlet %>% to_yamlet

to_yamlet.yamlet <- function(x,...)to_yamlet(unclass(x))

#'
#' Coerce Character to Yamlet Storage Format
#'
#' Coerces character to yamlet storage format.
#' Named character is processed as a named list.
#' NA_character_ is treated as the string 'NA'.
#' @param x character
#' @param ... ignored
#' @export
#' @keywords internal
#' @return length-one character
#' @family to_yamlet
#' @examples
#' to_yamlet('foo')
#' to_yamlet(c('a','b'))
#' to_yamlet(c(a = 'a',b = 'b'))
#' to_yamlet(c(no = 'n', yes = 'y'))
#' to_yamlet(NA)

to_yamlet.character <- function(x, ...){

  if(!is.null(names(x))){
    x <- as.list(x)
    return(to_yamlet(x))
  }

  # treat NA_character as 'NA'.
  x <- paste(x)

  # quote strings beginning with ' " [] {} > | * & ! % # ` @ ,. ? : -
  index <- grepl("^'", x)                       # starts with '
  x[index] <- paste0('"',x[index], '"')         # wrapped in "
  index <- grepl('^[][{}>|*&!%#`@,.?:-]', x)    # starts with special
  x[index] <- paste0("'",x[index],"'")          # wrapped in '

  # quote yes, no, y, n
  index <- x %in% c('yes','no','y','n')
  x[index] <- paste0("'",x[index],"'")

  # must quote existing ][, to disambiguate
  index <- grepl('[],[]', x)                   # contains collapse meta
  x[index] <- paste0("'",x[index],"'")         # wrapped in '

  # leading colon trapped above
  # must quote embedded colon-space (multi-char syntactical element)
  index <- grepl(': +', x)                   # contains collapse meta
  x[index] <- paste0("'",x[index],"'")         # wrapped in '


  if(length(x) == 1) return(x)

  # multiples get [,,]
  # collapse multiple
  x <- paste(x, collapse = ', ')
  x <- paste('[', x, ']')
  names(x) <- NULL # should not have names
  x
}

#' Coerce Null to Yamlet Storage Format
#'
#' Coerces null to yamlet storage format (returns empty string).
#' @param x object
#' @param ... ignored
#' @export
#' @keywords internal
#' @return length-one character
#' @family to_yamlet
#' @examples
#' to_yamlet(NULL)
to_yamlet.NULL <- function(x, ...)''

#' Coerce list to yamlet Storage Format
#'
#' Coerces list to yamlet storage format. Operates recursively on list members.
#' @param x object
#' @param ... ignored
#' @export
#' @keywords internal
#' @return length-one character
#' @family to_yamlet
#' @examples
#' to_yamlet(list())
#' to_yamlet(list(a = 1, b = 2, c = NULL))
#' to_yamlet(list(a = 1, b = list(c = 3, d = list(e = 4, f = 'g', 'h'))))
#' to_yamlet(setNames(1:3, c('a','b',NA)))
#' to_yamlet(setNames(c(1,2,NA), c('a','b','c')))
#'
to_yamlet.list <- function(x, ...){
  # convert each member to yamlet
  if(length(x) == 0) x <- list(NULL)
  nms <- names(x)
  nms <- sapply(nms, to_yamlet) # assures individual treatment
  out <- lapply(x, to_yamlet)
  # if member not null (''), attach name using colon-space,
  # else using ? name
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
  out <- unlist(out) # converts empty list to NULL
  if(is.null(out)) out <- ''

  if(length(out) == 1){ # a singlet

    # maybe *all* singlets need brackets

   # if(length(names(out))){ # not all singlets have names
   #  if(names(out) != ''){ # a singlet may have an empty name
        out <-paste0('[ ', out, ' ]') # named singlets need brackets
   #   }
   # }


  }


  if(length(out) > 1){ # sequences need brackets
    out <- paste(out, collapse = ', ')
    out <- paste0('[ ', out, ' ]')
  }
  out <- gsub('] ',']', out)
  names(out) <- NULL # should not have names
  out
}

#' Coerce Yamlet to Character
#'
#' Coerces yamlet to character.  See also \code{\link{as_yamlet.character}}.
#'
#' @param x yamlet
#' @param ... passed to \code{\link{as.character.yam}} and \code{\link{as_yam.yamlet}}
#' @export
#' @keywords internal
#' @family yamlet
#' @return character
#' @examples
#'
#' as.character(as_yamlet('ID: subject identifier'))
#' as.character(as_yamlet(c('id: subject','amt: dose')))
#' as.character(as_yamlet(c('id: subject\namt: dose')))
#' foo <- as_yamlet(system.file(package = 'yamlet', 'extdata','quinidine.yaml'))
#' class(foo)
#' writeLines(as.character(foo))
#' identical(foo, as_yamlet(as.character(foo)))
#' identical(as.character(foo), as.character(as_yamlet(as.character(foo))))

#' file <- system.file(package = 'yamlet','extdata','quinidine.csv')
#' file
#' foo <- resolve(decorate(file))
#' as.character(as_yamlet(foo))
#' as.character(as_yamlet(foo, exclude_attr = 'class'))
#'
as.character.yamlet <- function(x,...){
  y <- as_yam(x,...)
  z <- as.character(y, ...)
  z
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
#' meta <- system.file(package = 'yamlet', 'extdata','quinidine.yaml')
#' meta <- as_yamlet(meta)
#' list2encoding(meta$Creatinine$guide)
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

#' Encode Yamlet
#'
#' Encodes yamlet.  Each 'guide' element with length > 1
#' is converted to an encoding, if possible. If \code{data}
#' is supplied, conditional guides will be ignored.
#'
#' @param x yamlet
#' @param ... ignored
#' @param target attribute to encode
#' @param data optional data.frame for guide context
#' @return yamlet, with guide elements possibly transformed to encodings
#' @export
#' @keywords internal
#' @family encode
#' @examples
#' meta <- system.file(package = 'yamlet', 'extdata','quinidine.yaml')
#' meta <- as_yamlet(meta)
#' meta <- encode(meta)
encode.yamlet <- function(x, ..., target = 'guide', data = NULL){
  for(i in seq_along(x)){
    t <- x[[i]][[target]]
    if(!is.null(t)){
      if(length(t) > 1){ # prime criterion
        if(!is.null(data)){
          if(isConditional(t,data)){
            next
          }
        }
        try <- list2encoding(as.list(t))
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

#' Subset Yamlet
#'
#' Subsets yamlet. Preserves class, since a subset of yamlet is still yamlet.
#'
#' @param x object to subset
#' @param ... passed to next method
#' @return yamlet
#' @export
#' @keywords internal
#' @family yamlet
#' @examples
#' meta <- system.file(package = 'yamlet', 'extdata','quinidine.yaml')
#' meta <- as_yamlet(meta)
#' class(meta)
#' stopifnot(inherits(meta[1:2],'yamlet'))
`[.yamlet` <- function(x, ...){
  x <- NextMethod()
  class(x) <- 'yamlet'
  x
}


#' Print a Yamlet
#'
#' Prints a yamlet object for interactive inspection.
#'
#' @param x yamlet
#' @param ... ignored
#' @export
#' @keywords internal
#' @method print yamlet
#' @family yamlet
#' @return invisible(x)
#' @examples
#' as_yamlet('mpg: [efficiency, mi/gallon]\nvs: [Engine, [V-shaped: 0, straight: 1]]')

print.yamlet <- function(x, ...){
  render <- function(x, ...)UseMethod('render')
  render.list <- function(x, indent = 0, name = NULL, ...){
    margin <-  paste(rep(' ',indent), collapse = '')
    leader <- paste0(margin, '- ',name)
    writeLines(leader)
    for(i in seq_along(x)){
      render(x[[i]], indent = indent + 1, name = names(x)[[i]])
    }
  }
  render.default <- function(x, indent = 0, name = NULL, ...){
    margin <-  paste(rep(' ',indent), collapse = '')
    leader <- paste0(margin, '- ',name)
    data <- paste(format(x), collapse = ', ')
    msg <- paste0(leader,': ', data)
    writeLines(msg)
  }
  # render.function <- function(x, indent = 0, name = NULL, ...){
  #   margin <-  paste(rep(' ',indent), collapse = '')
  #   leader <- paste0(margin, '- ',name)
  #   data <- format(x)
  #   data <- paste0(margin, '    ', data)
  #   msg <- paste0(leader,': |')
  #   msg <- c(msg,data)
  #   writeLines(msg)
  # }
  if(!length(x)){
    writeLines('0 length object of class yamlet')
    invisible(x)
  }
  # x has length
  nms <- names(x)
  if(!length(nms))stop('yamlet must have names')
  # x has names
  lapply(seq_along(x), function(i)render(x[[i]], name = nms[[i]]))
  invisible(x)
}

#' Read Yamlet
#'
#' Reads yamlet from file.
#' Similar to \code{\link{io_yamlet.character}}
#' but also reads text fragments.
#'
#' @param x file path for yamlet, or vector of yamlet in storage syntax
#' @param ... passed to \code{\link{as_yamlet}}
#' @param default_keys character: default keys for the first n anonymous members of each element
#' @export
#' @family interface
#' @seealso \code{\link{decorate.data.frame}}
#' @return yamlet: a named list with default keys applied
#' @examples
#' library(csv)
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' meta <- system.file(package = 'yamlet', 'extdata','quinidine.yaml')
#' x <- as.csv(file)
#' y <- read_yamlet(meta)
#' x <- decorate(x, meta = y)
#' identical(x, decorate(file))
read_yamlet <- function(
  x,
  ...,
  default_keys = getOption(
    'yamlet_default_keys',
    list('label','guide')
  )
){
  stopifnot(is.character(x))
  as_yamlet(x, default_keys = default_keys, ...)
}
#' Write Yamlet
#'
#' Writes yamlet to file. Similar to \code{\link{io_yamlet.data.frame}}
#' but returns invisible storage format instead of invisible storage location.
#'
#' @param x something that can be coerced to class 'yamlet', like a yamlet object or a decorated data.frame
#' @param ... passed to \code{\link{as_yamlet}}
#' @param con passed to \code{\link{writeLines}}
#' @param eol end-of-line; passed to \code{\link{writeLines}} as \code{sep}
#' @param useBytes passed to \code{\link{writeLines}}
#' @param default_keys character: default keys for the first n anonymous members of each element
#' @param fileEncoding if \code{con} is character, passed to \code{\link{file}} as \code{encoding}
#' @export
#' @family interface
#' @seealso \code{\link{decorate.list}}
#' @return invisible character representation of yamlet (storage syntax)
#' @examples
#' library(csv)
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' meta <- system.file(package = 'yamlet', 'extdata','quinidine.yaml')
#' x <- as.csv(file)
#' y <- read_yamlet(meta)
#' x <- decorate(x, meta = y)
#' identical(x, decorate(file))
#' tmp <- tempfile()
#' write_yamlet(x, tmp)
#' identical(read_yamlet(meta), read_yamlet(tmp))

write_yamlet <- function(
  x,
  con = stdout(),
  eol = "\n",
  useBytes = FALSE,
  default_keys = getOption(
    'yamlet_default_keys',
    list('label','guide')
  ),
  fileEncoding = getOption('encoding'),
  ...
){
  x <- as_yamlet(x, default_keys = default_keys)
  y <- as.character(x, default_keys = default_keys, ...)
  if(is.character(con)){
    con <- file(con, 'w', encoding = fileEncoding)
    on.exit(close(con))
  }
  writeLines(text = y, con = con, sep = eol, useBytes = useBytes)
  invisible(y)
}
