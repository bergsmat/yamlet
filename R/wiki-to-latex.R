#' Convert One Wiki Symbol to Latex
#'
#' Converts one wiki symbol to latex.
#' See description for \code{\link{as_wikisymbol}}.
#' By default, unrecognized tokens are returned
#' unmodified.  However, Greek symbols are escaped.
#' See \code{\link{latexToken}}.
#'
#' @export
#' @keywords internal
#' @return character
#' @family wikisymbol
#' @param x character
#' @param unrecognized function to process unrecognized tokens: default \code{\link{latexToken}}
#' @param italics whether to use italics or not (default: no)
#' @param math whether to wrap in math environment (default: yes)
#' @param space latex sequence for single space
#' @param ... ignored
#' @examples
#' x <- c(
#'   'V_c./F',
#'   'AUC_ss',
#'   'C_max_ss',
#'   'var^eta_j'
#' )
#' x <- as_wikisymbol(x)
#' lapply(x, wiki_to_latex)
#' library(magrittr)
#' 'joule^\\*. ~1 kg m^2./s^2' %>% wiki_to_latex
#' 'gravitational force (kg\\.m/s^2.)' %>% wiki_to_latex
wiki_to_latex <- function(
  x,
  unrecognized = getOption('latex_unrecognized','latexToken'),
  italics = FALSE,
  math = TRUE,
  space = '\\:',
  ...
){
  # the latex of a wikisymol is the sequential
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
  if(!italics) y <- paste0('\\mathrm{', y, '}')
  if(math) y <- paste0('$', y, '$') # enforce math environment
  y
}

#' Process Latex Token
#'
#' Pre-processes a latex token not recognized as
#' wikisymbol.  By default, simply escapes the
#' common upper and lower case names for Greek letters.
#'
#' @param x character
#' @param ... ignored arguments
#' @param export
#' @keywords internal
#' @result character
#' @examples
#' latexToken('foo')
#' latexToken('alpha')
#' latexToken('Alpha')
latexToken <- function(x, ...){

greek <- c(
  'alpha','beta','gamma','delta','epsilon','zeta',
  'eta','theta','iota','kappa','lambda','mu',
  'nu','xi','omicron','pi','rho','sigma','tau',
  'upsilon','phi','chi','psi','omega'
)
Greek <- c(
  'Alpha','Beta','Gamma','Delta','Epsilon','Zeta',
  'Eta','Theta','Iota','Kappa','Lambda','Mu',
  'Nu','Xi','Omicron','Pi','Rho','Sigma','Tau',
  'Upsilon','Phi','Chi','Psi','Omega'
)
for(w in c(greek,Greek)){
  pattern <- paste0('\\b', w, '\\b')
  replacement <- paste0('\\\\', w)
  y <- gsub(pattern,replacement,x)
  y
}
}
