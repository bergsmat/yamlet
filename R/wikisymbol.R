#' Coerce to Wiki Symbol
#'
#' Coerces to class 'wikisymbol'. Generic,
#' with method \code{\link{as_wikisymbol.character}}.
#' A Wiki symbol is simple text with arbitrarily
#' nested subscript (\code{_}) and superscript
#' (\code{^}) groupings.  Use dot (\code{.})
#' to explicitly terminate a grouping.
#' Use asterisk(\code{*}) to suggest multiplication.
#' Escape specials with a backslash.
#' Trailing dots need not be supplied.
#' Leading/trailing whitespace is removed.
#' Tab character not allowed.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family wikisymbol
#' @return wikisymbol
#' @examples
#' example(as_wikisymbol.character)
as_wikisymbol <- function(x, ...)UseMethod('as_wikisymbol')

#' Coerce Character to Wiki Symbol
#'
#' Coerces character to class 'wikisymbol'.
#' See description for \code{\link{as_wikisymbol}}.
#'
#' @param x character
#' @param ... ignored arguments
#' @export
#' @family wikisymbol
#' @return wikisymbol
#' @examples
#' as_wikisymbol('V_c./F')
as_wikisymbol.character <- function(x, ...){
  if(any(grepl('\t', x))){
    stop('wikisymbol cannot contain tabs')
  }
  class(x) <- union('wikisymbol', class(x))
  x
}

#' Coerce Factor to Wiki Symbol
#'
#' Coerces factor to class 'wikisymbol'
#' by converting to character and calling
#' \code{\link{as_wikisymbol}}.
#'
#' @param x factor
#' @param ... ignored arguments
#' @export
#' @keywords internal
#' @family wikisymbol
#' @return wikisymbol
#' @examples
#' as_wikisymbol(as.factor('V_c./F'))
as_wikisymbol.factor <- function(x, ...)as_wikisymbol(as.character(x), ...)

#' Coerce to Plotmath
#'
#' Coerce to plotmath.  Generic, with method
#' \code{\link{as_plotmath.wikisymbol}}.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family wikisymbol
#' @return plotmath
#' @examples
#' example(as_plotmath.wikisymbol)
as_plotmath <- function(x, ...)UseMethod('as_plotmath')

#' Coerce to Latex
#'
#' Coerce to latex.  Generic, with method
#' \code{\link{as_latex.wikisymbol}}.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family wikisymbol
#' @return latex
#' @examples
#' example(as_latex.wikisymbol)
as_latex <- function(x, ...)UseMethod('as_latex')

#' Convert Wiki Symbol to Plotmath
#'
#' Converts wiki symbol to plotmath. See '?plotmath'.
#' Vectorized version of \code{\link{wikisym2plotmath_}}.
#'
#' @export
#' @param x wikisymbol
#' @param ... ignored
#' @return plotmath
#' @family wikisymbol
#' @examples
#' x <- c(
#'   'V_c./F',
#'   'AUC_ss',
#'   'C_max_ss',
#'   'var^eta_j'
#' )
#' x <- as_wikisymbol(x)
#' as_plotmath(x)
#' as_plotmath(as_wikisymbol('gravitational force (kg\\.m/s^2.)'))
as_plotmath.wikisymbol <- function(x, ...){
  y <- sapply(x, wikisym2plotmath_ , USE.NAMES = F)
  if(length(y) == 0) y <- character(0)
  class(y) <- union('plotmath', class(y))
  y
}
#' Convert Wiki Symbol to Latex
#'
#' Converts wiki symbol to latex.
#' Vectorized version of \code{\link{wikisym2latex_}}.
#'
#' @export
#' @param x wikisymbol
#' @param ... ignored
#' @return latex
#' @family wikisymbol
#' @examples
#' x <- c(
#'   'V_c./F',
#'   'AUC_ss',
#'   'C_max_ss',
#'   'var^eta_j'
#' )
#' x <- as_wikisymbol(x)
#' as_latex(x)
#' as_latex(as_wikisymbol('gravitational force (kg\\.m/s^2.)'))
as_latex.wikisymbol <- function(x, ...){
  y <- sapply(x, wikisym2latex_ , USE.NAMES = F)
  if(length(y) == 0) y <- character(0)
  class(y) <- union('latex', class(y))
  y
}

#' Convert One Wiki Symbol to Plotmath
#'
#' Converts one wiki symbol to plotmath.
#' See description for \code{\link{as_wikisymbol}}.
#' Space characters are converted to plotmath space
#' (\code{phantom(0)}). Asterix is converted to \code{\%.\%}.
#'
#' @export
#' @keywords internal
#' @return character
#' @family wikisymbol
#' @param x character
#' @param ... ignored
#' @examples
#' x <- c(
#'   'V_c./F',
#'   'AUC_ss',
#'   'C_max_ss',
#'   'var^eta_j'
#' )
#' x <- as_wikisymbol(x)
#' lapply(x, wikisym2plotmath_)
wikisym2plotmath_ <- function(x, ...){
  stopifnot(length(x) == 1)
  if(grepl('\t',x)) stop('tab character not allowed in wikisymbol')
  x <- sub('^\\s+','',x) # strip leading whitespace
  x <- sub('\\s+$','',x) # strip trailing whitespace
# x <- gsub('\\.', '\t',x,fixed = TRUE) # store literal dot as single character
  x <- strsplit(x,'')[[1]] # tokenize
  y <- character(0) # result accumulator
  b <- character(0) # closer stack
  while(length(x)){
    c <- x[1]
    x <- x[-1]
    t <- c # default
    # handle escapes
    if(c == '\\'){ # encountered escape character
      if(length(x)){ # still have characters remaining
        e <- x[1]
        if (e %in% c('\\','*','^', '_', '.')){ # attempt to escape something
          x <- x[-1] # pull it out of line
          t <- e
        }
      }
    }
    if(c == '_'){
      t <- '['  # subscript initiator
      b <- append(b,']') # subscript closer
    }
    if(c == '^'){
      t <- '^{' # superscript initiator
      b <- append(b,'}') # superscript closer
    }
    if(c == '.'){
      t <- b[length(b)]
      b <- b[-length(b)] # drop from stack
    }
  # if(c == '\t') t <- '.'  # literal dot
    if(c == '*') t <- '%.%'
    # if(c == ' ') t <- '*phantom(0)*'

    # accumulate
    y <- paste0(y,t)
  }

  # all characters handled
  # empty closer stack
  b <- paste(rev(b),collapse = '')
  y <- paste0(y,b)
  .reserved <- c(
    'if', 'else', 'repeat', 'while',
    'function', 'for', 'in', 'next', 'break',
    'TRUE', 'FALSE', 'NULL', 'Inf', 'NaN', 'NA', 'NA_integer_',
    'NA_real_', 'NA_complex_', 'NA_character_',
    '...', '%' #https://stackoverflow.com/questions/17334759/subscript-letters-in-ggplot-axis-label
  )
  # handle reserves
  s <- strsplit(y, ' ', fixed = TRUE)[[1]]
  s[s %in% .reserved] <- paste0('"', s[s %in% .reserved], '"') #  quote reserves
  y <- paste(y, collapse = '*phantom(0)*')
  y
}

#' Convert One Wiki Symbol to Latex
#'
#' Converts one wiki symbol to latex.
#' See description for \code{\link{as_wikisymbol}}.
#' These have special meaning in Wiki Symbol
#' and may be escaped with a backslash:
#' \code{\ * ^ _ .}.
#' These have special meaning in latex:
#' \code{& \% $ # _ { } ~ ^ \ }.
#' These have special meaning in both
#' wikisymbol and latex:
#' \code{\ ^ _ }.
#'
#' A Wiki symbol is simple text with arbitrarily
#' nested subscript (\code{_}) and superscript
#' (\code{^}) groupings.  Use dot (\code{.})
#' to explicitly terminate a grouping, and use
#' backslash-dot (\code{\.}) for a literal dot.
#' Examples: \code{V_c./F}. Trailing dots need
#' not be supplied. Leading/trailing whitespace
#' is removed. Tab character not allowed.
#'
#' @export
#' @keywords internal
#' @return character
#' @family wikisymbol
#' @param x character
#' @param ... ignored
#' @examples
#' x <- c(
#'   'V_c./F',
#'   'AUC_ss',
#'   'C_max_ss',
#'   'var^eta_j'
#' )
#' x <- as_wikisymbol(x)
#' lapply(x, wikisym2latex_)
wikisym2latex_ <- function(x, ...){
  stopifnot(length(x) == 1)
  if(grepl('\t',x)) stop('tab character not allowed in wikisymbol')
  x <- sub('^\\s+','',x) # strip leading whitespace
  x <- sub('\\s+$','',x) # strip trailing whitespace
# x <- gsub('\\.', '\t',x,fixed = TRUE) # store literal dot as single character
  x <- strsplit(x,'')[[1]] # tokenize
  y <- character(0) # result accumulator
  b <- character(0) # closer stack
  while(length(x)){
    c <- x[1]
    x <- x[-1]
    t <- c # default

    w <- c('\\',             '*',   '^',   '_', '.') # special in wiki
    w2<- c('\\textbackslash','*', '\\^', '\\_', '.') # use in latex

    l <- c(  '&',  '%',  '$',  '#',  '_',  '{',  '}','~',                '^',               '\\'             ) # special in latex
    l2<- c('\\&','\\%','\\$','\\#','\\_','\\{','\\}','\\textasciitilde','\\textasciicircum','\\textbackslash') # use in latex

    # handle escapes
    if(c == '\\'){ # encountered escape character
      if(length(x)){ # still have characters remaining
        e <- x[1]
        if (e %in% w){ # attempt to escape something
          x <- x[-1] # pull it out of line
          t <- w2[match(e, w)] # look up replacement
        }else{ # escape something unrecognized
          t <- e # pass-through
        }
      }else{ # terminal escape character
             # do nothing
      }
    }
    # handle wiki specials
    if(c == '_'){
      t <- '_{'  # subscript initiator
      b <- append(b,'}') # subscript closer
    }
    if(c == '^'){
      t <- '^{' # superscript initiator
      b <- append(b,'}') # superscript closer
    }
    if(c == '.'){
      t <- b[length(b)]
      b <- b[-length(b)] # drop from stack
    }
 #  if(c == '\t') t <- '.'  # literal dot
    # handle latex specials
    if(c %in% l){
      t <- l2[match(c, l)]
    }
    # accumulate
    y <- paste0(y,t)
  }

  # all characters handled
  # empty closer stack
  b <- paste(rev(b),collapse = '')
  y <- paste0(y,b)
  y
}



#' Coerce Plotmath to Expression
#'
#' Coerces plotath to expression by parsing as text.
#' @export
#' @keywords internal
#' @family wikisymbol
#' @param x plotmath
#' @param ... ignored arguments
#' @return expression, or list of expression for length(x) > 1
#' @examples
#' x <- c(
#'   'V_c./F',
#'   'AUC_ss',
#'   'C_max_ss',
#'   'var^eta_j'
#' )
#' x <- as_wikisymbol(x)
#' x <- as_plotmath(x)
#' x
#' as.expression(x)[[4]]
#' as.expression(x[[4]])
#' class(as.expression(x))
#' lapply(as.expression(x), class)
#' as.expression(as_plotmath(as_wikisymbol('V_c./F')))
#' as.expression(as_plotmath(as_wikisymbol(character(0))))
#' library(magrittr)
#' 'gravitational force (kg\\.m/s^2.)' %>%
#'   as_wikisymbol %>%
#'   as_plotmath %>%
#'   as_expression -> label
#'   label
#'
as.expression.plotmath <- function(x, ...)lapply(x, parse_one)

#' Coerce to Expression
#'
#' Coerces to expression.
#' Alias for \code{\link{as.expression}}.
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @return expression
as_expression <- function(x, ...)UseMethod('as.expression')

parse_one <- function(x){
  stopifnot(length(x) == 1)
  stopifnot(inherits(x, 'character'))
  y <- parse(text = x)
  y
}

#' Subset Wiki Symbol
#'
#' Subsets wikisymbol, retaining class.
#' @param x wikisymbol
#' @param ... passed to next method
#' @export
#' @keywords internal
#' @family wikisymbol
#' @return wikisymbol
#' @examples
#' x <- c(
#'   'V_c./F',
#'   'AUC_ss',
#'   'C_max_ss',
#'   'var^eta_j'
#' )
#' x <- as_wikisymbol(x)
#' class(x)
#' class(x[1])
`[.wikisymbol` <- function(x, ...){
  y <- NextMethod()
  # contrasts and levels will have been handled
  class(y) <- union('wikisymbol', class(y))
  y
}
#' Element-select Wiki Symbol
#'
#' Element-selects wikisymbol, retaining class.
#' @param x wikisymbol
#' @param ... passed to next method
#' @export
#' @keywords internal
#' @family wikisymbol
#' @return wikisymbol
#' @examples
#' x <- c(
#'   'V_c./F',
#'   'AUC_ss',
#'   'C_max_ss',
#'   'var^eta_j'
#' )
#' x <- as_wikisymbol(x)
#' class(x)
#' class(x[[1]])
`[[.wikisymbol` <- function(x, ...){
  y <- NextMethod()
  # contrasts and levels will have been handled
  class(y) <- union('wikisymbol', class(y))
  y
}

#' Subset Plotmath
#'
#' Subsets plotmath, retaining class.
#' @param x plotmath
#' @param ... passed to next method
#' @export
#' @keywords internal
#' @family wikisymbol
#' @return plotmath
#' @examples
#' x <- c(
#'   'V_c./F',
#'   'AUC_ss',
#'   'C_max_ss',
#'   'var^eta_j'
#' )
#' x <- as_plotmath(as_wikisymbol(x))
#' class(x)
#' class(x[1])
`[.plotmath` <- function(x, ...){
  y <- NextMethod()
  # contrasts and levels will have been handled
  class(y) <- union('plotmath', class(y))
  y
}
#' Element-select Plotmath
#'
#' Element-selects plotmath, retaining class.
#' @param x plotmath
#' @param ... passed to next method
#' @export
#' @keywords internal
#' @family wikisymbol
#' @return plotmath
#' @examples
#' x <- c(
#'   'V_c./F',
#'   'AUC_ss',
#'   'C_max_ss',
#'   'var^eta_j'
#' )
#' x <- as_plotmath(as_wikisymbol(x))
#' class(x)
#' class(x[[1]])
`[[.plotmath` <- function(x, ...){
  y <- NextMethod()
  # contrasts and levels will have been handled
  class(y) <- union('plotmath', class(y))
  y
}

##################

#' Subset Latex
#'
#' Subsets latex, retaining class.
#' @param x latex
#' @param ... passed to next method
#' @export
#' @keywords internal
#' @family wikisymbol
#' @return latex
#' @examples
#' x <- c(
#'   'V_c./F',
#'   'AUC_ss',
#'   'C_max_ss',
#'   'var^eta_j'
#' )
#' x <- as_latex(as_wikisymbol(x))
#' class(x)
#' class(x[1])
`[.latex` <- function(x, ...){
  y <- NextMethod()
  # contrasts and levels will have been handled
  class(y) <- union('latex', class(y))
  y
}
#' Element-select Latex
#'
#' Element-selects latex, retaining class.
#' @param x latex
#' @param ... passed to next method
#' @export
#' @keywords internal
#' @family wikisymbol
#' @return latex
#' @examples
#' x <- c(
#'   'V_c./F',
#'   'AUC_ss',
#'   'C_max_ss',
#'   'var^eta_j'
#' )
#' x <- as_latex(as_wikisymbol(x))
#' class(x)
#' class(x[[1]])
`[[.latex` <- function(x, ...){
  y <- NextMethod()
  # contrasts and levels will have been handled
  class(y) <- union('latex', class(y))
  y
}

#' Coerce Symbolic Units to Wiki Symbol.
#'
#' Coerces symbolic units to wikisymbol.  A literal dot
#' means different things in wikisymbol vs. units,
#' and there may be some other subtleties as well.
#' @param x units; see \code{\link[units]{as_units}}
#' @param ... ignored arguments
#' @export
#' @family wikisymbol
#' @return units
#' @examples
#' library(units)
#' x <- as_units('kg.m/s^2')
#' names(attributes(x))
#' y <- attr(x,'units')
#' class(y)
#' as.character(y)
#' as.character(attr(x, 'units'))
#' as_wikisymbol(y)
#' library(magrittr)
#' 'kg.m^2/s^2' %>% as_units %>% attr('units') %>% as_wikisymbol
as_wikisymbol.symbolic_units <- function(x, ...){
  y <- as.character(x)
  y <- gsub('\\.','*',y) # \u22c5 https://en.wikipedia.org/wiki/Interpunct
  y <- gsub('\\^([0-9])+','^\\1.',y)
  y <- as_wikisymbol(y)
  y
}

