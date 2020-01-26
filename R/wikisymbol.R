#' Convert Wiki Symbol to Plotmath
#'
#' Converts wiki symbol to plotmath.  Vectorized version of \code{\link{wikisym2plotmath_}}.
#'
#' @export
#' @keywords internal
#' @return expression
#' @family formatters
#' @param x character
#' @param ... ignored
wikisym2plotmath <- function(x,...){
  sapply(x, wikisym2plotmath_,...)
}

#' Convert One Wiki Symbol to Plotmath
#'
#' Converts one wiki symbol to plotmath.  A Wiki symbol is simple text with arbitrarily nested subscript (\code{_}) and superscript (\code{^}) groupings.  Use dot (\code{.}) to explicitly terminate a grouping, and use backslash-dot (\code{\.}) for a literal dot.  Examples: \code{V_c./F}. Trailing dots need not be supplied. Leading/trailing whitespace is removed. Tab character not allowed.
#'
#' @export
#' @return expression
#' @family formatters
#' @family lab
#' @param x character
#' @param ... ignored
#' @aliases wikisym wikisymbol
#' @examples
#' wikisym2plotmath_('V_c./F')
#' wikisym2plotmath_('AUC_ss')
#' wikisym2plotmath_('C_max_ss')
#' wikisym2plotmath_('var^eta_j')
wikisym2plotmath_ <- function(x,...){
  stopifnot(length(x) == 1)
  if(grepl('\t',x)) stop('tab character not allowed in wikisym')
  x <- sub('^\\s+','',x) # strip leading whitespace
  x <- sub('\\s+$','',x) # strip trailing whitespace
  x <- gsub('\\.', '\t',x,fixed = TRUE) # store literal dot as single character
  x <- strsplit(x,'')[[1]] # tokenize
  y <- character(0) # result accumulator
  b <- character(0) # closer stack
  while(length(x)){
    c <- x[1]
    x <- x[-1]
    t <- c # default
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
    if(c == '\t') t <- '.'  # literal dot

    # accumulate
    y <- paste0(y,t)
  }

  # all characters handled
  # empty closer stack
  b <- paste(rev(b),collapse = '')
  y <- paste0(y,b)
  y <- parse(text = y)
  y
}

# .reserved <- c(
#   'if', 'else', 'repeat', 'while',
#   'function', 'for', 'in', 'next', 'break',
#    'TRUE', 'FALSE', 'NULL', 'Inf', 'NaN', 'NA', 'NA_integer_',
#   'NA_real_', 'NA_complex_', 'NA_character_',
#   '...'
# )
