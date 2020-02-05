#' Convert One Wiki Symbol to Plotmath
#'
#' Converts one wiki symbol to plotmath.
#' See description for \code{\link{as_wikisymbol}}.
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
  tokenize <- function(x)strsplit(x,'')[[1]] # assumes length one character
  integral <- function(x)paste(x, collapse = '')
  nextNonSpaceIsToken <- function(x)grepl('^ *[-+]*[0-9a-zA-Z]', integral(x))
  nextCharIsSpace <- function(x)grepl('^ ', integral(x))
  lastNonSpaceIsToken <- function(x)grepl('^ *[]}0-9a-zA-Z"\']', integral(rev(tokenize(x))))
  lastCharIsSpace <- function(x)grepl('^ ', integral(rev(tokenize(x))))
  preop <- function(x){
    op <- ''
    if(lastNonSpaceIsToken(x) && lastCharIsSpace(x)) op <- '~'
    if(lastNonSpaceIsToken(x) && !lastCharIsSpace(x)) op <- '*'
    op
  }
  postop <- function(x){
    op <- ''
    if(nextNonSpaceIsToken(x) && nextCharIsSpace(x)) op <- '~'
    if(nextNonSpaceIsToken(x) && !nextCharIsSpace(x)) op <- '*'
    op
  }
  contextquote <- function(x, pre, post){# pre is length one accumulator, post is char vector
    paste0(preop(pre), '"', x, '"', postop(post))
  }
  stopifnot(length(x) == 1)
  if(grepl('\t',x)) stop('tab character not allowed in wikisymbol')
  x <- sub('^\\s+','',x) # strip leading whitespace
  x <- sub('\\s+$','',x) # strip trailing whitespace
  # x <- gsub('\\.', '\t',x,fixed = TRUE) # store literal dot as single character
  x <- tokenize(x)
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
          e <- contextquote(e, y, x) # quote it
          t <- e     # commit
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
    if(c == ' '){ # handle space
      if(nextNonSpaceIsToken(x)){
        t <- '~'
      } else {
        t <- '~' # leave as space
      }
    }
    if(c == '%') t <- contextquote(c, y, x)
    if(c == '~') t <- contextquote(c, y, x)
    if(c == '[') t <- '*"["*'
    if(c == ']') t <- '*"]"*'
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
    'NA_real_', 'NA_complex_', 'NA_character_'
  )  #, '...','%'
  #https://stackoverflow.com/questions/17334759/subscript-letters-in-ggplot-axis-label

  # handle reserves
  for(w in .reserved){
    pattern <- paste0('\\b', w, '\\b')
    replace <- paste0('"', w, '"')
    y <- gsub(pattern, replace, y)
  }
  # s <- strsplit(y, ' ', fixed = TRUE)[[1]]
  # s[s %in% .reserved] <- paste0('"', s[s %in% .reserved], '"') #  quote reserves
  # y <- paste(y, collapse = '*phantom(0)*')
  # context quoting can lead to clashes between prospective
  # and retrospective binding.
  y <- gsub('~*', '~', y, fixed = TRUE)
  y <- gsub('*~', '~', y, fixed = TRUE)
  y <- sub('\\*$', '', y)
  y <- sub('\\~$', '', y)
  y
}
