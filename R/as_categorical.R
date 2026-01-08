globalVariables(c('guide', '.'))

#' Coerce to Categorical
#' 
#' Coerce to categorical. 
#' Generic, with method \code{\link{as_categorical.decorated}}.
#' 
#' @export
#' @keywords internal
#' @family decorated
#' @param x object of dispatch
#' @param ... passed
as_categorical <- function(x, ...) UseMethod('as_categorical')

#' Coerce decorated to Categorical
#' 
#' Coerces (elements of) 'decorated' to categorical.  See example(s).
#' Briefly, a continuous variable with units (and a small set of existing values)
#' is converted to a factor-ready variable.
#' 
#' @export
#' @keywords manip
#' @family decorated
#' @param x decorated
#' @param ... unquoted names of columns to be converted
#' @return decorated
#' @examples
#' library(magrittr)
#' library(tablet)
#' library(kableExtra)
#' library(yamlet)
#' x <- data.frame(DOSE = c(12, 1.2, 2.4, 6, 12, 1.2)) %>% decorate('DOSE: [ Dose, mg ]')
#' x %>% 
#'   as_categorical(DOSE) %>%
#'   resolve %>% 
#'   tablet %>% 
#'   as_kable %>%
#'   kable_classic
as_categorical.decorated <- function(x, ...)modify(x, ..., guide = .as_categorical(x, guide))

.as_categorical <- function(x, guide)structure(guide, names = paste(as.list(sort(unique(x)))))

