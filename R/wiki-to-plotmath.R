#' Convert One Wiki Symbol to Plotmath
#'
#' Converts one wiki symbol to plotmath.
#' See description for \code{\link{as_wikisymbol}}.
#' By default, unrecognized tokens are returned
#' unmodified if they are parseable.
#' Otherwise, backslashes and single quotes are escaped,
#' and the result is wrapped in single quotes.
#' See \code{\link{plotmathToken}}.
#'
#' @export
#' @keywords internal
#' @return character
#' @family wikisymbol
#' @param x character
#' @param unrecognized function to process unrecognized tokens: default \code{\link{plotmathToken}}
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
#' render <- . %>% as_wikisymbol %>% as_plotmath %>% as.expression
#' '^*' %>% render
#' '^\\*' %>% render
#' '^\\*.' %>% render
#' '^\\.' %>% render
#' '^\\\\' %>% render
#' '\\\\' %>% render
#' '^\\^' %>% render
#' '^\\_' %>% render
#' '\\^' %>% render
#' '^.a' %>% render
#' '\\$' %>% render
#' wiki_to_plotmath('\\$') %>% goodToken
#' wiki_to_plotmath('\\$', unescape = FALSE) %>% goodToken
#' wiki_to_plotmath('\\$', unrecognized = function(x,...)x) %>% goodToken
#' options(plotmath_unrecognized = function(x,...)x)
#' wiki_to_plotmath('\\$') %>% goodToken
#' options(plotmath_unrecognized = NULL)
#' wiki_to_plotmath('\\$') %>% goodToken
#'
wiki_to_plotmath <- function(
  x,
  unrecognized = getOption('plotmath_unrecognized','plotmathToken'),
  ...
){
  # the plotmath of a wikisymol is the sequential
  # combination of tokens.
  # tokens are separated by _ or ^ or . are non-printing
  # but trigger nesting or un-nesting.
  # Single quote is escaped and used for quoting.
  # Whitespace is quoted.
  # \\, \., \*, \_, and \^ are quoted.
  # unescaped '*' is promoted to %.%.
  # surviving tokens are processed by 'unrecognized'.

  x <- wikitoken(x,...)
  closers <- character(0)
  active <- FALSE
  if(length(x)==0)return(x)
  if(identical(x, ''))return(x)
  base <- ''
  explicit <- c(
    '\\s+',
    '[*]','[.]','[_]','\\^',
    '[\\][*]','[\\][.]','[\\][_]','[\\]\\^'
  )
  for(token in x){
    m <- sapply(explicit, function(pattern)position(token, pattern))
    if(max(m) == -1){ # unrecognized token
      # pre-process
      fun <- match.fun(unrecognized)
      token <- fun(token, ...)
      if(active){
        base <- paste0(base, '*', token)
      }else{
        if(grepl('[]}]$',base)){ # not empty nest
          base <- paste0(base, '*', token)
          active <- TRUE
        }else{ # empty nest or start of line
          base <- paste0(base, token)
          active <- TRUE
        }
      }
    }
    if(max(m) != -1){ # recognized token
      m <- m[m != -1]
      m <- m[m == min(m)]
      stopifnot(length(m) == 1)
      p <- names(m)
      if(p == '\\s+'){
        token <- paste0("'",token,"'")
        if(active){
          base <- paste0(base, '*', token)
        }else{
          if(grepl('[]}]$',base)){ # not empty nest
            base <- paste0(base, '*', token)
            active <- TRUE
          }else{ # empty nest or start of line
            base <- paste0(base, token)
            active <- TRUE
          }
        }
      }
      if(p == '[\\][*]'){
        token <- paste0("'*'")
        if(active){
          base <- paste0(base, '*', token)
        }else{
          base <- paste0(base, token)
          active <- TRUE
        }
      }
      if(p == '[\\][.]'){
        token <- paste0("'.'")
        if(active){
          base <- paste0(base, '*', token)
        }else{
          base <- paste0(base, token)
          active <- TRUE
        }
      }
      if(p == '[\\][_]'){
        token <- paste0("'_'")
        if(active){
          base <- paste0(base, '*', token)
        }else{
          base <- paste0(base, token)
          active <- TRUE
        }
      }
      if(p == '[\\]\\^'){
        token <- paste0("'^'")
        if(active){
          base <- paste0(base, '*', token)
        }else{
          base <- paste0(base, token)
          active <- TRUE
        }
      }
      if(p == '[*]'){
        token <- paste0("%.%")
        if(active){
          base <- paste0(base, token)
          active <- FALSE
        }else{
          base <- paste0(base, "''", token)
          active <- FALSE
        }
      }
      if(p == '[.]'){
        if(length(closers)){
          cl <- closers[[1]]
          closers <- closers[-1]
          if(grepl('%\\.%$',base)) base <- paste0(base, "''")
          if(active){
            base <- paste0(base, cl)
            active <- FALSE
          }else{ # not active
            if(grepl('[[{]$',base)){# empty nest ok
              base <- paste0(base, cl)
            }else{
              base <- paste0(base, cl )
            }
          }
        }
      }
      if(p == '[_]'){
        closers <- c(']', closers)
        if(active){
          base <- paste0(base, "[")
          active <- FALSE
        }else{
          if(!grepl('[]}]', base)){
            # must have something to subscript
            base <- paste0(base, "''[")
          }else{
            base <- paste0(base, "*''[")
          }
        }
      }
      if(p == '\\^'){
        closers <- c('}', closers)
        if(active){
          base <- paste0(base, "^{")
          active <- FALSE
        }else{
          if(!grepl('[]}]', base)){
            # must have something to superscript
            base <- paste0(base, "''^{")
          }else{
            base <- paste0(base, "*''^{")
          }
        }
      }
    }
  }
  # use of %.% can leave a dangling operator.
  # supply default rhs before closing
  # indeed, always check for %.% before appending close
  if(grepl('%\\.%$',base)) base <- paste0(base, "''")
  if(length(closers)){ # dump
    if(grepl('%\\.%$',base)) base <- paste0(base, "''")
    if(active){
      base <- paste0(base, paste(closers, collapse = ''))
    }else{
      if(grepl('[[{]',base)){
        # empty script ok
        base <- paste0(base, paste(closers, collapse = ''))
      }else{
        base <- paste0(base, paste(closers, collapse = ''))
      }
    }
  }
  return(base)
}

#' Test Whether Token is Parseable
#'
#' Tests whether token is Parseable
#' @param x length-one character
#' @param ... ignored arguments
#' @return logical
#' @export
#' @keywords internal
#' @examples
#' goodToken('alpha')
#' goodToken('foo')
#' goodToken('\\$')
goodToken <- function(x,...){
  stopifnot(length(x) == 1)
  y <- try(silent = TRUE, parse(text = x))
  if(inherits(y, 'try-error'))return(FALSE)
  TRUE
}

#' Quote a Token
#'
#' Quotes a token. Escapes single-quotes and wraps in single-quotes.
#' @param x (length-one) character
#' @param conditional if true, return good tokens (parseable) unmodified
#' @param unescape whether to escape (unrecognized) backslash
#' @param ... ignored arguments
#' @export
#' @keywords internal
#' @return character
#' @examples
#' plotmathToken("can't")
#' plotmathToken("\\$")
#' plotmathToken("\\$", unescape = FALSE)
#' plotmathToken("\\$") %>% goodToken
#' options(plotmath_unescape = FALSE)
#' plotmathToken("\\$") %>% goodToken
#' options(plotmath_unescape = NULL)
#' plotmathToken("\\$", unescape = FALSE) %>% goodToken
#' plotmathToken('foo')
#' plotmathToken('foo',conditional = FALSE)
#' options('plotmath_conditional_quote' = FALSE)
#' plotmathToken('foo')
#' options('plotmath_conditional_quote' = NULL)
#' plotmathToken('foo')

plotmathToken <- function(
  x,
  conditional = getOption('plotmath_conditional_quote', TRUE),
  unescape = getOption('plotmath_unescape', TRUE),
  ...
){
  token <- x
  if(conditional){
    if(goodToken(token)){
      return(token)
    }
  }
  if(unescape) token <- gsub('[\\]','\\\\\\\\', token)
  token <- gsub("'","\\\\'",token)
  token <- paste0("'", token, "'")
  token
}

#' Render Wiki Symbol in Plot
#'
#' For diagnostic purposes, render wikisymbol in a ggplot.
#' @param x wikisymbol; see \code{\link{as_wikisymbol}}
#' @param ... ignored arguments
#' @export
#' @keywords internal
#' @import ggplot2
#' @examples
#' wiki_to_gg('1 joule^\\*. ~1 kg m^2./s^2')
wiki_to_gg <- function(x, ...){
  p <- ggplot(data.frame(x = 1,y = 1,label = wiki_to_plotmath(x)))
  p <- p + geom_text(aes(x,y,label=label), parse = TRUE)
  p <- p +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0))
  p <- p + theme(axis.line=element_blank(),axis.text.x=element_blank(),
            axis.text.y=element_blank(),axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),legend.position="none",
            panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),plot.background=element_blank())
  p
}










