#' Drop Title
#'
#' Drop title attribute.
#' Generic, with methods 
#' \code{\link{drop_title.default}},
#' \code{\link{drop_title.decorated}}, and
#' \code{\link{drop_title.dvec}}.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family deprecated
#' @family labels
#' @return see methods
#' @examples
#' # see methods
drop_title <- function(x, ...)UseMethod('drop_title')

#' Drop Title for Decorated
#' 
#' Drops title for 'decorated' class.
#' Limits scope to requested variables, and then calls
#' class-specific methods for each.
#'
#' @param x object
#' @param ... optional names of variables to limit scope
#' @export
#' @keywords internal
#' @family labels
#' @return decorated
#' @examples
#' library(magrittr)
#' x <- data.frame(length = 1:10)
#' x %>% 
#'   decorate('length: [ Length, mm ]') %>%
#'   resolve %>% 
#'   desolve %>%
#'   decorations
#'
drop_title.decorated <- function(
  x,
  ...
){
  vars <- selected(x, ...)
  args <- named(...)
  for(var in vars){
    # pass only named arguments
    x[[var]] <- do.call(drop_title, c(list(x[[var]]),args))
  }
  x
}

#' Drop Title by Default
#' 
#' Drops title by default.
#' To be specific:  this is the default method
#' for the generic function \code{\link{drop_title}},
#' and it actually does nothing.  Individual methods
#' are written for those classes where 'drop title' 
#' behavior is expected.
#' 
#' @param x object
#' @param ... ignored
#' @export
#' @keywords internal
#' @family labels
#' @return same as x
drop_title.default <- function(
    x,
    ...
){
  return(x)
}

#' Drop Title for Decorated Vector
#' 
#' Drops title for decorated vectors.
#' If option \code{with_title} is TRUE
#' and x has a 'units' attribute,
#' it removes the title attribute. See also 
#' \code{\link{make_title}} for coordinated use.
#' @param x dvec
#' @param ... ignored arguments
#' @param with_title whether to drop title
#' @export
#' @keywords internal
#' @family labels
#' @return dvec
#' @examples
#' library(magrittr)
#' 1 %>% 
#' as_dvec(label = 'length', guide = 'mm') %>%
#' resolve %>%
#' desolve
drop_title.dvec <- function(
    x,
    ...,
    with_title = getOption('yamlet_with_title', TRUE)
){
  stopifnot(length(with_title) == 1)
  with_title <- as.logical(with_title)
  if(with_title & 'units' %in% names(attributes(x))){
    attr(x, 'title') <- NULL
  }
  x
}
