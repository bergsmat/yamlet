#' Coerce Guide to Something More Explicit
#'
#' Coerces 'guide' to something more explicit.  Generic, with methods for
#' data.frame and yamlet.  The key 'guide' generally suggests a guide
#' to interpretation of a data item, such as units, formats, codelists,
#' and encodings.  The idea here is to replace 'guide' with something
#' explicit in case required downstream.
#'
#' @param x object of dispatch
#' @param ... passed arguments
#' @export
#' @return see methods
#' @family explicit_guide
#' @md
explicit_guide <- function(x,...)UseMethod('explicit_guide')

#' Coerce Yamlet Guide to Something More Explicit
#'
#' Coerces yamlet 'guide' keys to something more explicit.
#' The key 'guide' generally suggests a guide
#' to interpretation of a data item, such as units, formats, codelists,
#' and encodings.  The idea here is to replace 'guide' with something
#' explicit in case required downstream.
#'
#' The key 'guide' is replaced as follows for the first test
#' that succeeds, or replaced with the default if none do.
#'
#' * If the value of 'guide' is of length greater than
#' one (and data is not supplied), key becomes 'codelist'.
#' * If the value of 'guide' \code{\link{is_parseable}},
#' key becomes 'units'.
#' * If the value of 'guide' contains two or more percent signs,
#' key becomes 'format' (i.e. a 'format' string for a date or time class).
#' * If the value of 'guide' is (\code{\link[encode]{encoded}}),
#' key becomes 'encoding'.
#'
#' If \code{data} is supplied, guides with length greater than one
#' are checked to see if they evaluate to conditions in data context
#' (see \code{\link{isConditional.list}}).
#' If so, inferences are based on the first guide element rather
#' than the guide as a whole.
#'
#'
#' @param x yamlet
#' @param default length-one character: the default key
#' @param data optional data.frame for testing guides with length > 1
#' @param ... ignored arguments
#' @export
#' @keywords internal
#' @importFrom dplyr case_when
#' @importFrom encode encoded
#' @return yamlet
#' @family explicit_guide
#' @examples
#' library(magrittr)
#' 'CONC: [ concentration, µg/mL ]' %>% as_yamlet %>% explicit_guide
#' 'RACE: [ subject race, [ Caucasian: 0, Latin: 1, Black: 2 ]]' %>% as_yamlet %>% explicit_guide
#' 'RACE: [ subject race, //0/Caucasian//1/Latin//2/Black// ]' %>% as_yamlet %>% explicit_guide
#' 'DATE: [ date, "%Y-%m-%d" ]' %>% as_yamlet %>% explicit_guide
#' 'PRSE: [ standard error, "%" ]' %>% as_yamlet %>% explicit_guide
#'
explicit_guide.yamlet <- function(x, default = 'guide', data = NULL, ...){
  stopifnot(is.character(default), length(default) == 1)
  if(!is.null(data))stopifnot(is.list(data))
  for(i in seq_along(x)){
    nms <- names(x[[i]])
    for(j in seq_along(x[[i]])){
      if(length(nms) >= j){
        if(nms[[j]] == 'guide'){
          val <- x[[i]][[j]]
          if(length(val) > 1){ # may be a conditional
            if(!is.null(data)){
              if(isConditional(val, data)){
                val <- val[[1]] # just test the first element
              }
            }
          }
          explicit <- case_when(
            length(val) > 1 ~ 'codelist',
            is_parseable(val) ~ 'units',
            length(gregexpr(pattern = '%', val)[[1]]) > 1 ~ 'format',
            encoded(val) ~ 'encoding',
            TRUE ~ default
          )
          explicit <- unique(explicit)
          names(x[[i]])[[j]] <- explicit
        }
      }
    }
  }
  x
}

#' Coerce Data Frame Guide to Something More Explicit
#'
#' Coerces data.frame 'guide' attributes to something more explicit.
#' The attribute 'guide' generally suggests a guide
#' to interpretation of a data item, such as units, formats, codelists,
#' and encodings.  The idea here is to replace 'guide' with something
#' explicit in case required downstream.
#'
#' This method pulls the 'decorations' off of the data.frame,
#' converts to yamlet, applies \code{\link{explicit_guide.yamlet}},
#' purges 'guide' attributes from the data.frame,
#' and then re-decorates using \code{overwrite = TRUE}.
#'
#' @param x yamlet
#' @param overwrite passed as TRUE
#' @param ... passed to \code{\link{as_yamlet}}, \code{\link{explicit_guide}}, and \code{\link{decorate}}
#' @export
#' @keywords internal
#' @importFrom dplyr case_when
#' @importFrom encode encoded
#' @return yamlet
#' @family explicit_guide
#' @examples
#' library(magrittr)
#' x <- data.frame(
#'  ID = 1,
#'  CONC = 1,
#'  RACE = 1,
#'  SEX = 1,
#'  DATE = 1
#' )
#' x$ID   %<>% structure(label = 'subject identifier')
#' x$CONC %<>% structure(label = 'concentration', guide = 'ng/mL')
#' x$RACE %<>% structure(label = 'race', guide = list(white = 0, black = 1, asian = 2))
#' x$SEX  %<>% structure(label = 'sex', guide = list(female = 0, male = 1))
#' x$DATE %<>% structure(label = 'date', guide = '%Y-%m-%d')
#' x %>% as_yamlet
#' x %>% explicit_guide %>% as_yamlet
explicit_guide.data.frame <- function(x, overwrite = TRUE, ...){
  y <- as_yamlet(x,...)
  y <- explicit_guide(y, data = x, ...)
  for(nm in names(x)){
    attr(x[[nm]], 'guide') <- NULL
  }
  x <- decorate(x, meta = y, overwrite = TRUE, ...)
  x
}

#' Check Parseable as Units
#'
#' Checks if something is parseable as units.
#' Generic, with default method.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @return see methods
#' @family parseable
#' @examples
#' example(is_parseable.character)
is_parseable <- function(x,...)UseMethod('is_parseable')


#' Check Something is Parseable as Units by Default
#'
#' Checks if something is parseable as units.
#' Tests against the udunits library in \pkg{units}.
#' See \code{\link[units]{as_units}}.
#' See also \code{\link[units]{install_symbolic_unit}}
#' for finer control.
#'
#' @param x character
#' @param ... passed arguments
#' @export
#' @importFrom units as_units
#' @return logical
#' @family parseable
#' @family interface
#' @examples
#' is_parseable(c('kg/m2','kg/m^2','kg.m/s2','µg/L'))
#' is_parseable('foo')
#' library(units)
#' install_symbolic_unit('foo')
#' is_parseable('foo')
#'
is_parseable.default <- function(x,...){
  res <- sapply(x, .is_parseable.default, ..., USE.NAMES = FALSE)
  res
}

.is_parseable.default <- function(x,...){
  stopifnot(length(x) == 1)
  res <- try(
    silent = TRUE,
    suppressWarnings(
      suppressMessages(
        as_units(x,...)
      )
    )
  )
  res <- !inherits(res, 'try-error')
  res
}


