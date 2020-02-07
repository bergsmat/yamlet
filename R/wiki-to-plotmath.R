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
#' lapply(x, wiki_to_plotmath)
#' library(magrittr)
#' wiki_to_plotmath('^*') %>% parse(text = .)
#' wiki_to_plotmath('^\\*') %>% parse(text = .)
#' wiki_to_plotmath('^\\*.') %>% parse(text = .)
#' wiki_to_plotmath('^\\.') %>% parse(text = .)
#' wiki_to_plotmath('^\\\\') %>% parse(text = .)
#' wiki_to_plotmath('\\\\') %>% parse(text = .)
#' wiki_to_plotmath('^\\^') %>% parse(text = .)
#' wiki_to_plotmath('^\\_') %>% parse(text = .)
#' wiki_to_plotmath('\\^') %>% parse(text = .)
wiki_to_plotmath <- function(x, close = character(0), ...){
  # the plotmath of a wikisym is the recursive
  # combination of leading and trailing tokens.
  # tokens are separated by _ or ^ or .
  # single quote is escaped and used for quoting
  # leading whitespace is quoted and attached with *
  # trailing whitespace is quoted and attached with *
  # \\, \., \*, \_, and \^ are quoted and attached with *
  # unescaped '*' is promoted to %.%.
  # surviving tokens are returned verbatim if parseable
  # and single-quoted if not.
  stack <- paste(rev(close), collapse = '')         # consolidate stack
  stopifnot(inherits(x, 'character'))
  stopifnot(length(x) == 1)
  # any non-operators, opt. operator, anything else
  pre  <- sub('^([^\\._^]*)([._^]?)(.*)$', '\\1', x)
  op   <- sub('^([^\\._^]*)([._^]?)(.*)$', '\\2', x) # does not see '.' in '\\*.'
  post <- sub('^([^\\._^]*)([._^]?)(.*)$', '\\3', x)

  # Maxim:  there is always a pre.
  if(x == '' && length(close) == 0)return("''")

  # handle early closure
  if(op == '.'){
    if(!length(close))return(wiki_to_plotmath(paste0(pre,post), ...))
    early  <- close[[length(close)]]
    close <- close[-length(close)]
    lhs <- wiki_to_plotmath(pre,  close = early, ...)
    rhs <- wiki_to_plotmath(post, close = close, ...)
    if(rhs == "''") rhs <- '' # 'post' not essential
    if(lhs == '' & rhs == '') return("''")
    if(lhs != '' & rhs == '') return(lhs)
    if(lhs == '' & rhs != '') return(rhs)
    #if(lhs != '' & rhs != '') return(paste0(lhs, rhs))
    if(lhs != '' & rhs != '' & post != '') return(paste0(lhs, '*', rhs))
    # if there was no substance in 'post',
    # then there is nothing for 'early' to skip past,
    # and nothing to do but concatenate closers
    if(lhs != '' & rhs != '' & post == '') return(paste0(lhs, rhs))
  }

  # handle superscript
  if(op == '^'){
    close  <- c(close, '}')
    lhs <- wiki_to_plotmath(pre,...)
    rhs <- wiki_to_plotmath(post, close = close, ...)
    out <- paste0(lhs, '^{', rhs)
    return(out)
  }

  # handle subscript
  if(op == '_'){
    close  <- c(close, ']')
    lhs <- wiki_to_plotmath(pre,...)
    rhs <- wiki_to_plotmath(post, close = close, ...)
    out <- paste0(lhs, '[', rhs)
    return(out)
  }

  # All operators have been handled, close etc. no longer relevant
  rm(pre)
  rm(post)
  rm(op)

  # handle mixed whitespace
  if(grepl('^\\s+\\S', x)){ # leading white
    lhs <- sub('^(\\s+)(.*)$','\\1',x)
    rhs <- sub('^(\\s+)(.*)$','\\2',x)
    return(paste0(wiki_to_plotmath(lhs, ...), '*', wiki_to_plotmath(rhs, close = close, ...)))
  }

  if(grepl('^\\S+\\s', x)){ # leading non-white
    lhs <- sub('^(\\S+)(.*)$','\\1',x)
    rhs <- sub('^(\\S+)(.*)$','\\2',x)
    return(paste0(wiki_to_plotmath(lhs, ...), '*', wiki_to_plotmath(rhs, close = close, ...)))
  }

  # handle pure whitespace

  if(grepl('^\\s+$', x)) return(paste0("'", x, "'", stack )) # not recursive!

  # anything getting this far is free of operators and whitespace

  # handle escapes
  # at this point, we have just a token with no whitespace, operators, or nesting

  if(grepl('\\\\[._^*\\]', x)){
    # any number of non backslash, a backslash, a target, and anything else
    pre  <- sub('^([^\\]*)\\\\([._^*\\])(.*)$', '\\1', x)
    nop  <- sub('^([^\\]*)\\\\([._^*\\])(.*)$', '\\2', x)
    post <- sub('^([^\\]*)\\\\([._^*\\])(.*)$', '\\3', x)
    lhs <- wiki_to_plotmath(pre, ...)
    rhs <- wiki_to_plotmath(post, close = close, ...)
    if(rhs == "''") rhs <- ''
    if(nop == '\\') nop <- '\\\\'
    nop <- paste0("'", nop, "'")
    out <- nop
   # browser()
    if(pre != '') out <- paste0(lhs, '*', out)
    if(rhs != ''){
      if(rhs == paste(close, collapse = '')){
        out <- paste0(out, rhs) # merely need to close
      }else{ # apparently there is additional material
        out <- paste0(out, '*', rhs)
      }
    }
    return(out)
  }

  # promote unescaped * (no escaped * can be present)
  if(grepl('[*]', x)){
    pre  <- sub('^([^*]*)[*](.*)$', '\\1', x)
    post <- sub('^([^*]*)[*](.*)$', '\\2', x)
    # %.% is a binary op, needs two args
    # pre is positive definite
    # make post so
    if(post == '') post <- "''"
    lhs <- wiki_to_plotmath(pre, ...)
    rhs <- wiki_to_plotmath(post, close = close, ...)
    #if(rhs == "''") rhs <- '' # simplify
    return(paste0(lhs,'%.%',rhs))
  }

  # anything surviving this far has no whitespace, operators, nesting, escapes,  or *
  # parse if possible, else quote

  # handle unparseable
  test <- try(silent = TRUE, parse(text = x))
  if(inherits(test, 'try-error')){                  # unparseable
    x <- gsub("'",'\\\'',x, fixed = TRUE)           # escape single quotes
    x <- paste0("'", x, "'") # quote unparseable    # enclose with single quotes
  }
  x <- paste0(x, stack)                             # dump stack
  return(x)
}




