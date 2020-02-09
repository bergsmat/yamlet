#' Convert One Wiki Symbol to Latex
#'
#' Converts one wiki symbol to latex.
#' See description for \code{\link{as_wikisymbol}}.
#' By default, unrecognized tokens are returned
#' literally.  However, Greek symbols are escaped.
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
#' 'V_c./F' %>% wiki_to_latex
wiki_to_latex <- function(
  x,
  unrecognized = getOption('latex_unrecognized','latexToken'),
  italics = FALSE,
  math = TRUE,
  ...
){
  # the latex of a wikisymol is the sequential
  # combination of tokens.
  # tokens separated by _ or ^ or . are non-printing
  # but trigger nesting or un-nesting.
  # Whitespace and recognized escapes are supplied literally.
  # unescaped '*' is promoted to \code{\cdot}.
  # surviving tokens are processed by 'unrecognized',
  # which escapes Greek characters and renders other
  # tokens literally.

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
        base <- paste0(base, ' ', token)
      }else{
        if(grepl('[]}]$',base)){ # not empty nest
          base <- paste0(base, ' ', token)
          active <- TRUE
        }else{ # empty nest or start of line
          base <- paste0(base,' ', token)
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
        token <- paste0("\\textrm{",token,"}")
        if(active){
          base <- paste0(base, ' ', token)
        }else{
          if(grepl('[]}]$',base)){ # not empty nest
            base <- paste0(base, ' ', token)
            active <- TRUE
          }else{ # empty nest or start of line
            base <- paste0(base, ' ', token)
            active <- TRUE
          }
        }
      }
      if(p == '[\\][*]'){
        token <- paste0("\\textrm{*}")
        if(active){
          base <- paste0(base, ' ', token)
        }else{
          base <- paste0(base, ' ', token)
          active <- TRUE
        }
      }
      if(p == '[\\][.]'){
        token <- paste0("\\textrm{.}")
        if(active){
          base <- paste0(base, ' ', token)
        }else{
          base <- paste0(base, ' ', token)
          active <- TRUE
        }
      }
      if(p == '[\\][_]'){
        token <- paste0("\\textrm{_}")
        if(active){
          base <- paste0(base, ' ', token)
        }else{
          base <- paste0(base, ' ', token)
          active <- TRUE
        }
      }
      if(p == '[\\]\\^'){
        token <- paste0("\\textrm{^}")
        if(active){
          base <- paste0(base, ' ', token)
        }else{
          base <- paste0(base, ' ', token)
          active <- TRUE
        }
      }
      if(p == '[*]'){
        token <- paste0("{\\cdot}")
        if(active){
          base <- paste0(base, ' ', token)
          active <- FALSE
        }else{
          base <- paste0(base, ' ', token)
          active <- FALSE
        }
      }
      if(p == '[.]'){
        if(length(closers)){
          cl <- closers[[1]]
          closers <- closers[-1]
          #if(grepl('%\\.%$',base)) base <- paste0(base, "''")
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
        closers <- c('}', closers)
        if(active){
          base <- paste0(base,"_{")
          active <- FALSE
        }else{
          if(!grepl('[]}]', base)){
            # must have something to subscript
            base <- paste0(base, "~_{")
          }else{
            base <- paste0(base, "~_{")
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
            base <- paste0(base, "~^{")
          }else{
            base <- paste0(base, "~^{")
          }
        }
      }
    }
  }
  # use of %.% can leave a dangling operator.
  # supply default rhs before closing
  # indeed, always check for %.% before appending close
  #if(grepl('%\\.%$',base)) base <- paste0(base, "''")
  if(length(closers)){ # dump
    #if(grepl('%\\.%$',base)) base <- paste0(base, "''")
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
  if(!italics) base <- paste0('\\mathrm{', base, '}')
  if(math) base <- paste0('$', base, '$') # enforce math environment
  return(base)
}

#' Process Latex Token
#'
#' Pre-processes a latex token not recognized as
#' wikisymbol.  By default, simply escapes the
#' common upper and lower case names for Greek letters.
#'
#' @param x character
#' @param ... ignored arguments
#' @export
#' @keywords internal
#' @return character
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
  if(x %in% c(greek, Greek)){
    x <- paste0('\\', x)
  }else{
    x <- paste0('\\textrm{',x, '}')
  }
  x
}

#' Preview Something
#'
#' Creates a preview.
#' Generic, with methods for latex and plotmath.
#' @param x object
#' @param ... passed arguments
#' @export
#' @family preview
#' @return see methods
#' @examples
#' library(magrittr)
#' 'V_c./F' %>% as_wikisymbol %>% as_plotmath %>% as_preview
#' \dontrun{
#' 'V_c./F' %>% as_wikisymbol %>% as_latex %>% as_preview
#' }
#' 'Omega ~ joule^\\*. ~1 kg*m^2./s^2' %>% as_wikisymbol %>% as_plotmath %>% as_preview
#' \dontrun{
#' 'Omega ~ joule^\\*. ~1 kg*m^2./s^2' %>% as_wikisymbol %>% as_latex %>% as_preview
#' }
as_preview <- function(x, ...)UseMethod('as_preview')

#' Preview Wiki Symbol as Latex
#'
#' Preview wikisymbol after conversion to latex.
#' Creates and displays a temporary png file, after
#' conversion from pdf using \code{\link[latexpdf]{ghostconvert}}.
#' @param x wikisymbol; see \code{\link{as_wikisymbol}}
#' @param wide nominal page width
#' @param long nominal page length
#' @param dir a working directory; see \code{\link[latexpdf]{as.pdf}}
#' @param gs_cmd ghostscript command; see \code{\link[latexpdf]{ghostconvert}}
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family preview
#' @keywords internal
#' @importFrom latexpdf as.png
#' @importFrom latexpdf as.pdf
#' @importFrom latexpdf ghostconvert
#' @importFrom png readPNG
#' @importFrom grid grid.raster
#' @return invisible filepath
#' @examples
#' \dontrun{
#' library(magrittr)
#' 'Omega ~ joule^\\*. ~1 kg*m^2./s^2' %>%
#' as_wikisymbol %>%
#' as_latex %>%
#' as_preview
#' }
as_preview.latex <- function(
  x,
  wide = 50,
  long = 20,
  stem = 'latex_preview',
  dir = tempdir(),
  gs_cmd = 'mgs',
  ...
){
   stopifnot(length(x) == 1)
   #x <- wiki_to_latex(x, ...)
   pdf <- as.pdf(x, stem = stem, dir = dir, wide = wide, long = long, ...)
   png <- ghostconvert(pdf, gs_cmd = gs_cmd, ...)
   img <- readPNG(png)
   grid.raster(img)
   invisible(png)
}

#' Compare Plotmath and Latex Previews
#'
#' Compares plotmath and latex previews of wikisymbol
#' Generates png for both, and overlays
#' latex above plotmath.
#'
#' @param x length-one character (will be coerced to wikisymbol)
#' @param wide width in mm of the latex image
#' @param long length in mm of the latex image
#' @param width width (default: inches) of the plotmath image
#' @param height height (default: inches) of the plotmath image
#' @param ... passed arguments
#' @export
#' @return invisible list of filepaths
#' @family preview
#' @examples
#' specials <- '& % $ # \\_ { } ~ \\^ \\\\ '
#' '& % $ ~ \\_ { } \\^ \\\\ #' %>% as_wikisymbol %>% as_plotmath %>% goodToken
#' specials %>% as_wikisymbol %>% as_latex
#' compare(specials)
compare <- function(x, wide = 50, long = 20, width = 3, height = 1,...){
  stopifnot(length(x) == 1)
  stopifnot(inherits(x, 'character'))
  x <- as_wikisymbol(x)
  a <- as_preview(as_plotmath(x))
  b <- as_preview(as_latex(x))
  invisible(list(plotmath = a, latex = b))
}

