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
#' library(magrittr)
#' 'joule^\\*. ~1 kg m^2./s^2' %>% wikisym2latex_
#' 'gravitational force (kg\\.m/s^2.)' %>% wikisym2latex_
wikisym2latex_ <- function(x, ...){
  tokenize <- function(x)strsplit(x,'')[[1]] # assumes length one character
  stopifnot(length(x) == 1)
  if(grepl('\t',x)) stop('tab character not allowed in wikisymbol')
  x <- sub('^\\s+','',x) # strip leading whitespace
  x <- sub('\\s+$','',x) # strip trailing whitespace
  # x <- gsub('\\.', '\t',x,fixed = TRUE) # store literal dot as single character
  x <- tokenize(x)
  y <- character(0) # result accumulator
  b <- character(0) # closer stack
    w <- c('\\',             '*',                 '^',   '_', '.') # special in wiki
    w2<- c('{\\textbackslash}','*', '{\\textasciicircum}', '\\_', '.') # use in latex

    l <- c(  '&',  '%',  '$',  '#',  '_',  '{',  '}','~',                '^',               '\\'             ) # special in latex
    l2<- c('\\&','\\%','\\$','\\#','\\_','\\{','\\}','{\\sim}','{\\textasciicircum}','{\\textbackslash}') # use in latex
  while(length(x)){
    c <- x[1]
    x <- x[-1]
    t <- c # default
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
    # if(c == '\t') t <- '.'  # literal dot
    if(c == '*') t <- '{\\cdot}'
    if(c == ' ') t <- '{\\:}'
    # handle latex specials
    if(c %in% setdiff(l,w)){
      t <- l2[match(c, l)]
    }
    # accumulate
    y <- paste0(y,t)
  }

  # all characters handled
  # empty closer stack
  b <- paste(rev(b),collapse = '')
  y <- paste0(y,b)
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
    y <- gsub(pattern,replacement,y)
  }
  y <- paste0('$\\mathrm{', y, '}$') # enforce math environment
  y
}

