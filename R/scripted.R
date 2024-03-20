#' Render Scripted Attributes of Indicated Components
#'
#' Renders the scripted attributes of indicated components.
#' Generic, with method \code{\link{scripted.default}}.
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @return see methods
#' @family scripted
#' @seealso modify.default
#' @seealso as_spork
#' @examples
#' example(scripted.default)
scripted <- function(x, ...)UseMethod('scripted')

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
#' In addition to the 'title' and 'expression' attributes, scripted() writes
#' a 'plotmath' attribute to store plotmath versions of factor levels, 
#' where present. \code{\link{print.decorated_ggplot}} should prefer
#' these over their latex and html counterparts.
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
#' \code{scripted()} always calls \code{resolve()} for the indicated
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
#' @return same class as x
#' @family scripted
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
#' x %>% scripted %>% ggplot(aes(time, work)) + geom_point()
#' x %>% scripted(format = 'html') %$% work %>% attr('title')

scripted.default <- function(
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
  
  x <- resolve(x, ...)
  vars <- selected(x, ...)
  for(var in vars){
    label <- attr(x[[var]], 'label')
    units <- attr(x[[var]], 'units')
    
    # https://github.com/r-quantities/units/issues/221
    # explicitly spork-terminate all superscripts immediately following the integer
    
    # render factor levels where present
    if(!is.null(levels(x[[var]]))){
      attr(x[[var]], 'plotmath') <- levels(x[[var]]) %>% as_spork %>% as_plotmath
      if(format == 'latex'){
        levels(x[[var]]) %<>% as_spork %>% as_latex
        class(x[[var]]) <- c(class(x[[var]]), 'latex')
      }
      if(format == 'html'){
        levels(x[[var]]) %<>% as_spork %>% as_html
        class(x[[var]]) <- c(class(x[[var]]), 'html')
      }
    }
    
    if(!is.null(units)){
      units <- gsub('(\\^[-]?[0-9]+)','\\1.', units)
    }
    if(!is.null(units)) units <- c(open, units, close) # nulls disappear!
    result <- c(label, units) # nulls disappear!
    usable <- result
    if(is.null(result)) usable <- ''
    if(format == 'latex'){
      title = concatenate(spork::as_latex(as_spork(usable)))
    }
    if(format == 'html'){
      title = concatenate(spork::as_html(as_spork(usable)))
    }
    plotm = concatenate(as_plotmath(as_spork(usable)))
    
    # token not null, title not null, plotm not null
    if(!is.null(result)) attr(x[[var]], 'title') <- title # ready to use
    plotm <- as.expression(plotm)
    attr(plotm, 'srcref') <- NULL
    attr(plotm, 'srfile') <- NULL
    attr(plotm, 'wholeSrcref') <- NULL
    if(!is.null(result)) attr(x[[var]], 'expression') <- plotm #as.expression(plotm)
  }
  x
}

# @importFrom spork htmlToken
# @export
# spork::htmlToken

# @importFrom spork as_html
# @export
# spork::as_html

# @importFrom spork as_latex
# @export
# spork::as_latex

# @importFrom spork as_plotmath
# @export
# spork::as_plotmath

# @importFrom spork concatenate
# @export
# spork::concatenate

# @importFrom spork as_spork
# @export
# spork::as_spork

# @importFrom dplyr group_vars
# @export
# dplyr::group_vars


