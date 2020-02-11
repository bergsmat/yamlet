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
  gs_cmd = getOption('gs_cmd','mgs'),
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

#' Compare Previews
#'
#' Compare previews of something.
#' Generic, with method \code{\link{as_previews.wikisymbol}}.
#' @param x object
#' @param ... passed arguments
#' @export
#' @return see methods
#' @family preview
#' @examples
#' example(as_previews.wikisymbol)
as_previews <- function(x,...)UseMethod('as_previews')

#' Compare Previews of Wiki Symbol
#'
#' Compares plotmath and latex previews of wikisymbol
#' Generates png for both, and overlays
#' latex above plotmath.
#'
#' @param x length-one wikisymbol
#' @param wide width in mm of the latex image
#' @param long length in mm of the latex image
#' @param width width (default: inches) of the plotmath image
#' @param height height (default: inches) of the plotmath image
#' @param ... passed arguments
#' @export
#' @return invisible list of filepaths
#' @family preview
#' @examples
#' library(magrittr)
#' specials <- '& % $ # \\_ { } ~ \\^ \\'
#' specials %>% gsub(' ','',.) %>% as_wikisymbol %>% as_previews
#' specials %>% as_wikisymbol %>% as_previews
#' as_previews(as_wikisymbol('$'))-> foo

as_previews.wikisymbol <- function(x, wide = 13, long = 2.54, width = .25, height = 0.05,...){
  stopifnot(length(x) == 1)
  stopifnot(inherits(x, 'character'))
  x <- as_wikisymbol(x)
  a <- as_preview(as_plotmath(x))
  b <- as_preview(as_latex(x))
  invisible(list(plotmath = a, latex = b))
}

