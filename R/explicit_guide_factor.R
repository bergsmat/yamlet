
#' Coerce Factor Guide to Something More Explicit
#'
#' Coerces factor 'guide' attribute to something more explicit.
#' The attribute 'guide' generally suggests a guide
#' to interpretation of a data item, such as units, formats, codelists,
#' and encodings.  The idea here is to replace 'guide' with something
#' explicit in case required downstream. Generally a codelist is expected, but not enforced.
#'
#' @param x factor
#' @param ... named arguments passed to \code{\link{as_yamlet}}, \code{\link{explicit_guide}}, and \code{\link{decorate}}; un-named arguments ignored
#' @param overwrite whether to overwrite attributes
#' @param simplify whether to remove guide attribute
#' @param expand whether to expand empty guide list using sorted unique values. NA likely excluded.
#' @export
#' @keywords internal
#' @importFrom dplyr case_when
#' @importFrom encode encoded
#' @return factor
#' @family explicit_guide
#' @examples
#' library(magrittr)
#' x <- data.frame(
#'  ID = as_dvec(1),
#'  SEX = factor(1)
#' )
#' x %<>% modify(SEX, label = 'sex', guide = list(female = 0, male = 1))
#' x %>% decorations
#' x %>% explicit_guide %>% decorations
#' x %>% explicit_guide(SEX) %>% decorations # limit scope
#' x %$% SEX %>% explicit_guide
explicit_guide.factor <- function(
    x,
    ...,
    overwrite = getOption('yamlet_explicit_guide_overwrite',TRUE),
    simplify = getOption('yamlet_explicit_guide_simplify', TRUE),
    expand = getOption('yamlet_expand_codelist', TRUE)
){
  stopifnot(is.logical(overwrite), length(overwrite) == 1)
  stopifnot(is.logical(simplify), length(simplify) == 1)
  stopifnot(is.logical(expand), length(expand) == 1)
  y <- data.frame(x = x)
  y <- do.call(
    explicit_guide,
    c(
      list(
        x = y, 
        overwrite = overwrite, 
        simplify = simplify,
        expand = expand
      ),
      named(...)
    )
  )
  y <- y$x
  y
}

#' Coerce Factor Guide to Something More Implicit
#'
#' Coerces factor guide-like attributes to 'guide'.
#' The attribute 'guide' generally suggests a guide
#' to interpretation of a data item, such as units, formats, codelists,
#' and encodings.  The idea here is to replace these with 'guide':
#' i.e., to undo the effects of \code{\link{explicit_guide.dvec}}.
#' If guide attribute is still present, the explicit attribute is removed.
#' Otherwise the explicit element is renamed.
#'
#'
#' @param x factor
#' @param ... ignored
#' @export
#' @keywords internal
#' @importFrom dplyr case_when
#' @importFrom encode encoded
#' @return factor
#' @family explicit_guide
#' @family dvec
#' @examples
#' library(magrittr)
#' x <- data.frame(RACE = factor(1))
#' x %<>% modify(RACE, label = 'race', guide = list(white = 0, black = 1, asian = 2))
#' x %>% decorations
#' x %>% explicit_guide %>% decorations
#' x %>% explicit_guide %>% implicit_guide %>% decorations
#' x %>% explicit_guide(simplify = FALSE) %>% decorations
#' x %>% explicit_guide(simplify = FALSE) %>% implicit_guide %>% decorations
#' x %<>% explicit_guide
#' a <- x$RACE
#' str(a)
#' str(a %>% implicit_guide)
implicit_guide.factor <- function(
    x,
    ...
){
  y <- data.frame(x = x)
  y <- implicit_guide(y)
  y <- y$x
  y
}

