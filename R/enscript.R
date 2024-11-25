#' Render Scripted Attributes of Indicated Components
#'
#' Renders the scripted attributes of indicated components.
#' Generic, with method \code{\link{enscript.default}}.
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @return see methods
#' @family enscript
#' @seealso modify.default
#' @seealso as_spork
#' @examples
#' example(enscript.default)
enscript <- function(x, ...)UseMethod('enscript')

#' Render Scripted Attributes of Indicated Components by Default
#'
#' Modifies specific attributes of each indicated element
#' (all elements by default).
#' 
#' The goal here is to render labels and units (where present)
#' in a way that supports subscripts and superscripts 
#' for both plots and tables in either html or latex contexts.
#' 
#' The current implementation writes an 'expression' attribute
#' to support figure labels and a 'title' attribute to support
#' tables. \code{\link{print.decorated_ggplot}} will attempt
#' to honor the expression attribute if it exists.
#' \code{\link[tablet]{tablet.data.frame}} will attempt to honor
#' the title attribute if it exists (see Details there).
#' An attempt is made to guess the output format (html or latex).
#' 
#' In addition to the 'title' and 'expression' attributes, enscript() writes
#' a 'plotmath' attribute to store plotmath versions of factor levels, 
#' where present. \code{\link{print.decorated_ggplot}} should prefer
#' these over their latex and html counterparts. Furthermore,
#' factor levels (and codelists, where present) are converted
#' to their latex or html equivalents. None of this happens
#' if a 'plotmath' attribute already exists, thus preventing 
#' the same variable from being accidentally transformed twice.
#' 
#' To flexibly support latex, html, and plotmath, this function
#' expects column labels and units to be encoded in "spork" syntax.
#' See \code{\link[spork]{as_spork}} for details and examples.
#' Briefly, "_" precedes a subscript, "^" precedes a superscript,
#' and "." is used to force the termination of either a 
#' superscript or a subscript where necessary. For best results,
#' units should be written using *, /, and ^; e.g. "kg*m^2/s^2"
#' not "kg m2 s-2" (although both are valid:  
#' see \code{\link{is_parseable}}). A literal backslash followed by "n"
#' represents a newline. Greek letters are represented by their names,
#' except where names are enclosed in backticks.
#' 
#' 
#' \code{enscript()} always calls \code{resolve()} for the indicated
#' columns, to make units present where appropriate.
#' 
#' @param x object
#' @param ... indicated columns, or name-value pairs; passed to \code{\link{resolve}} and \code{\link{selected}}
#' @param open character to precede units
#' @param close character to follow units
#' @param format one of 'latex' or 'html'
#' @export
#' @importFrom knitr is_latex_output
#' @importFrom spork as_html as_latex as_plotmath concatenate htmlToken as_spork
#' @return 'enscript', a superclass of x
#' @family enscript
#' @family interface
#' @examples
#' library(magrittr)
#' library(ggplot2)
#' x <- data.frame(time = 1:10, work = (1:10)^1.5)
#' x %<>% decorate('
#'   time: [ Time_elapsed, h ]
#'   work: [ Work_total_observed, kg*m^2/s^2 ]
#' ')
#' 
#' x %>% decorations
#' x %>% ggplot(aes(time, work)) + geom_point()
#' x %>% enscript %>% ggplot(aes(time, work)) + geom_point()
#' x %>% enscript(format = 'html') %$% work %>% attr('title')
#' testthat::expect_equal(enscript(x), enscript(enscript(x)))

enscript.default <- function(
    x, 
    ...,
    open = getOption("yamlet_append_units_open", " ("),
    close = getOption("yamlet_append_units_close", ")"),
    format = getOption("yamlet_format", ifelse(knitr::is_latex_output(), 'latex','html'))
    
){
  stopifnot(
    is.character(format), 
    length(format) == 1, 
    format %in% c('latex','html')
  )
  
  stopifnot(is.character(open) | is.null(open))
  stopifnot(is.character(close) | is.null(close))
  stopifnot(length(open) %in% 0:1)
  stopifnot(length(close) %in% 0:1)
  
  x <- resolve(x, ...) # removes html or latex tail where present
  vars <- selected(x, ...)
  for(var in vars){
    label <- attr(x[[var]], 'label')
    units <- attr(x[[var]], 'units')
    
    
    
    # render factor levels where present and untouched
    # for idempotency, use presence of plotmath attribute
    # as evidence of prior rendering
    if(
      !is.null(levels(x[[var]])) & 
      is.null(attr(x[[var]], 'plotmath'))
    ){
      attr(x[[var]], 'plotmath') <- as_plotmath(as_spork(levels(x[[var]])))
      if(format == 'latex'){
        levels(x[[var]]) <- as_latex(as_spork(levels(x[[var]])))
        class(x[[var]]) <- union(class(x[[var]]), 'latex')
      }
      if(format == 'html'){
        levels(x[[var]]) <- as_html(as_spork(levels(x[[var]])))
        class(x[[var]]) <- union(class(x[[var]]), 'html')
      }
      # need to maintain internal consistency of 'classified'
      # ideally, there should be a method for this
      if(inherits(x[[var]], 'classified')){
        for(i in seq_along(levels(x[[var]]))){
          attr(x[[var]], 'codelist')[[i]] <- levels(x[[var]])[[i]]
        }
      }
    }
    
    # for idempotency, ensure tail is restored for factor-like vars
    if(!is.null(levels(x[[var]]))){
      if(format == 'latex'){
        class(x[[var]]) <- union(class(x[[var]]), 'latex')
      } else {
        class(x[[var]]) <- union(class(x[[var]]), 'html')
      }
    }
    
    # explicitly spork-terminate all sub, super in label
    if(!is.null(label)){
      dots <- gsub('\\\\.','',label)
      dots <- gsub('[^.]', '', dots)
      dots <- nchar(dots)
      
      subs <- gsub('\\\\_','',label)
      subs <- gsub('[^_]', '', subs)
      subs <- nchar(subs)
      
      sups <- gsub('\\\\^','',label)
      sups <- gsub('[^^]', '', sups)
      sups <- nchar(sups)
      
      need <- subs + sups - dots
      need <- max(0, need)
      
      tail <- rep('.', need)
      tail <- paste(tail, collapse = '')
      
      label <- paste0(label, tail)
    }
    
    # https://github.com/r-quantities/units/issues/221
    # explicitly spork-terminate all superscripts 
    # immediately following the integer
    if(!is.null(units)){
      units <- gsub('(\\^[-]?[0-9]+)','\\1.', units)
    }
    
    
    if(!is.null(units)) units <- c(open, units, close) # nulls disappear!
    result <- c(label, units) # nulls disappear!
    usable <- result
    if(is.null(result)) usable <- ''
    usable <- paste(usable, collapse = '') # new
    usable <- as_spork(usable)
    if(format == 'latex'){
      # title = concatenate(spork::as_latex(usable))
      title = spork::as_latex(usable)
    }
    if(format == 'html'){
      # title = concatenate(spork::as_html(usable))
      title = spork::as_html(usable)
    }
    # plotm = concatenate(as_plotmath(usable))
    plotm = as_plotmath(usable)
    
    # token not null, title not null, plotm not null
    if(!is.null(result)) attr(x[[var]], 'title') <- title # ready to use
    plotm <- as.expression(plotm)
    attr(plotm, 'srcref') <- NULL
    attr(plotm, 'srcfile') <- NULL
    attr(plotm, 'wholeSrcref') <- NULL
    if(!is.null(result)) attr(x[[var]], 'expression') <- plotm #as.expression(plotm)
  }
  #class(x) <- c('enscript', class(x))
  x
}
