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
#' @keywords internal
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
#' If \code{data} is supplied, guides that are lists
#' are checked to see if they evaluate to conditions in data context
#' (see \code{\link{isConditional.list}}).
#' If so, inferences are based on the first guide element rather
#' than the guide as a whole.
#'
#' This method iterates across the guide elements, renaming them
#' as specified by the value of \code{test}. (default: \code{\link{infer_guide}}).
#' \code{test} should be a function (or name of one) that accepts x, data, and dots.
#' If a data.frame is passed to explicit_guide(), the relevant column will
#' be passed as data to \code{test}.

#' @param x yamlet
#' @param ... passed to \code{\link[dplyr]{select}} to limit scope
#' @param test function or function name; supply non-default or globally set \code{options(yamlet_infer_guide = )}.
#' @param expand whether to expand empty guide list using sorted unique values. NA likely excluded.
#' @param data optional data.frame for testing guides with length > 1
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
#' 'RACE: [ subject race, [ Caucasian, Latin, Black ]]' %>% as_yamlet %>% explicit_guide
#' 'RACE: [ subject race, //0/Caucasian//1/Latin//2/Black// ]' %>% as_yamlet %>% explicit_guide
#' 'DATE: [ date, "%Y-%m-%d" ]' %>% as_yamlet %>% explicit_guide
#' 'PRSE: [ standard error, "%" ]' %>% as_yamlet %>% explicit_guide
#'
explicit_guide.yamlet <- function(
  x, ...,
  test = getOption('yamlet_infer_guide', yamlet::infer_guide),
  expand = getOption('yamlet_expand_codelist', TRUE),
  data = NULL
){
  if(!is.null(data))stopifnot(is.list(data))

  ### don't do all, just selected names

# for(i in seq_along(x)){
  for(i in selected(x, ...)){
      nms <- names(x[[i]])
    for(j in seq_along(x[[i]])){
      if(length(nms) >= j){
        if(nms[[j]] == 'guide'){
          val <- x[[i]][[j]]
          if(expand){
            if(!is.null(val)){
              if(is.list(val)){
                if(length(val) == 0){
                  val <- as.list(sort(unique(data[[i]])))
                  x[[i]][[j]] <- val
                }
              }
            }
          }
          if(length(val) > 1){ # may be a conditional
            if(!is.null(data)){
              if(isConditional(val, data)){
                val <- val[[1]] # just test the first element
              }
            }
          }
          explicit <- match.fun(test)(val, data = data[[i]], token = i, ...)
          stopifnot(is.character(explicit), length(explicit) == 1)
          # explicit <- unique(explicit)
          names(x[[i]])[[j]] <- explicit
        }
      }
    }
  }
  x
}

#' Infer Type of Guide
#'
#' Infers type of guide.
#' Default mapping function for \code{\link{explicit_guide.yamlet}}
#' where it replaces the key 'guide' with the return value.
#'
#' * If x is a list, the result is 'codelist'.
#'
#' * If x otherwise has length greater than 1, result is the default value.
#'
#' * If x \code{\link{is_parseable}}, result is 'units'.
#'   Use \code{\link[units]{install_unit}} to register a non-default unit.
#'
#' * If x contains two or more percent signs, result is 'format'
#'   (i.e. a 'format' string for a date or time class).
#'
#' * If x is (\code{\link[encode]{encoded}}), result is 'encoding'.
#'
#' * A length-one value of x not otherwise recognized is
#'   assumed to be an attempt to provide a length-one 'codelist'.
#'
#' If data is supplied (not NULL), a warning is issued for
#' a codelist with elements not present.
#'
#' @param x character or list
#' @param data atomic
#' @param default value for unrecognized guides
#' @param token character: discriptive term for 'data' used in warning
#' @param ... ignored
#' @return length-one character
#' @export
#' @keywords internal
#' @family explicit_guide
#' @examples
#' infer_guide('a') # recognized unit
#' infer_guide('z') # unrecognized as unit, evaluates to guide
#' \dontrun{
#' # evaluates to codelist but data suggests otherwise (warning)
#' infer_guide(as.list(letters), data = LETTERS)
#' }
#' infer_guide(c(1,2,3))                   # guide
#' infer_guide(list('a','b','c'))          # codelist
#' infer_guide(list(a = 1, b = 2, c = 3))  # codelist
#' infer_guide(list(a = 1))                # codelist
#' infer_guide('kg/m^2')                   # units
#' infer_guide('%')                        # units
#' infer_guide('%Y-%m-%d')                 # format
#' infer_guide('//a/1//b/2//c/3//')        # encoding
#'
infer_guide <- function(
  x,
  data = NULL,
  default = 'guide',
  token = 'data',
  ...
){
  stopifnot(is.atomic(data)||is.null(data))
  stopifnot(length(token) == 1)
  token <- as.character(token)
  # TTB @1.1.3 to support % test on empty list
  formatted <- function(x){
    if(length(x) == 0) return(FALSE)
    percents <- function(x)nchar(gsub('[^%]', '', x))
    all(percents(x) > 1)
  }
  res <- case_when(

    # a list is clearly an attempt to supply a codelist
    is.list(x) ~ 'codelist',

    # all codelist now parsed as list @ 0.8.2
    # not expecting any further plurality
    length(x) > 1 ~ default,

    # now everything is length one
    # registered unit?  (units::install_unit())
    all(is_parseable(x)) ~ 'units',

    # two percent signs?
    #length(gregexpr(pattern = '%', x)[[1]]) > 1 ~ 'format',
    formatted(x) ~ 'format',

    # qualifies as encoding?
    all(encoded(x)) ~ 'encoding',

    # x is length-one, none of the above
    TRUE ~ default
  )
  stopifnot(length(res) == 1)
  undescribed <- setdiff(data, x)
  if(length(x) > 10){
    x[[11]] <- '...'
    x <- x[1:11]
  }
  msg <- paste(collapse = ', ', unlist(x))
  if(identical(res, 'codelist') & length(undescribed))warning(
    token, ' has values not in ', msg, ': e.g. ', undescribed[[1]]
  )
  res
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
#' @param x data.frame
#' @param ... named arguments passed to \code{\link{as_yamlet}}, \code{\link{explicit_guide}}, and \code{\link{decorate}}; un-named arguments limit scope
#' @param overwrite passed as TRUE
#' @param simplify whether to remove guide attribute
#' @param expand whether to expand empty guide list using sorted unique values. NA likely excluded.
#' @export
#' @keywords internal
#' @importFrom dplyr case_when
#' @importFrom encode encoded
#' @return data.frame
#' @family explicit_guide
#' @family dvec
#' @examples
#' library(magrittr)
#' x <- data.frame(
#'  ID = 1,
#'  CONC = 1,
#'  RACE = 1,
#'  SEX = 1,
#'  DATE = 1
#' )
#' x %<>% modify(ID, label = 'subject identifier')
#' x %<>% modify(CONC, label = 'concentration', guide = 'ng/mL')
#' x %<>% modify(RACE, label = 'race', guide = list(white = 0, black = 1, asian = 2))
#' x %<>% modify(SEX, label = 'sex', guide = list(female = 0, male = 1))
#' x %<>% modify(DATE, label  = 'date', guide = '%Y-%m-%d')
#' x %>% decorations
#' x %>% explicit_guide %>% decorations
#' x %>% explicit_guide(DATE) %>% decorations # limit scope
explicit_guide.data.frame <- function(
    x,
    ...,
    overwrite = getOption('yamlet_explicit_guide_overwrite',TRUE),
    simplify = getOption('yamlet_explicit_guide_simplify', TRUE),
    expand = getOption('yamlet_expand_codelist', TRUE)
){
  stopifnot(is.logical(overwrite), length(overwrite) == 1)
  stopifnot(is.logical(simplify), length(simplify) == 1)
  y <- do.call(as_yamlet, c(list(x), named(...)))
  nms <- selected(x, ...)
  y <- y[as.character(nms)] # selected may have incompatible class path
  y <- do.call(explicit_guide, c(list(y, expand = expand, data = x), named(...)))
  if(simplify){
    for(nm in nms){
      attr(x[[nm]], 'guide') <- NULL
    }
  }
  x <- do.call(decorate, c(list(x, meta = y, overwrite = TRUE), named(...)))
  x
}

#' Coerce Decorated Vector Guide to Something More Explicit
#'
#' Coerces dvec 'guide' attribute to something more explicit.
#' The attribute 'guide' generally suggests a guide
#' to interpretation of a data item, such as units, formats, codelists,
#' and encodings.  The idea here is to replace 'guide' with something
#' explicit in case required downstream.
#'
#' @param x dvec
#' @param ... named arguments passed to \code{\link{as_yamlet}}, \code{\link{explicit_guide}}, and \code{\link{decorate}}; un-named arguments ignored
#' @param overwrite whether to overwrite attributes
#' @param simplify whether to remove guide attribute
#' @param expand whether to expand empty guide list using sorted unique values. NA likely excluded.
#' @export
#' @keywords internal
#' @importFrom dplyr case_when
#' @importFrom encode encoded
#' @return dvec
#' @family explicit_guide
#' @examples
#' library(magrittr)
#' x <- data.frame(
#'  ID = as_dvec(1),
#'  CONC = as_dvec(1),
#'  RACE = as_dvec(1),
#'  SEX = as_dvec(1),
#'  DATE = as_dvec(1)
#' )
#' x %<>% modify(ID, label = 'subject identifier')
#' x %<>% modify(CONC, label = 'concentration', guide = 'ng/mL')
#' x %<>% modify(RACE, label = 'race', guide = list(white = 0, black = 1, asian = 2))
#' x %<>% modify(SEX, label = 'sex', guide = list(female = 0, male = 1))
#' x %<>% modify(DATE, label  = 'date', guide = '%Y-%m-%d')
#' x %>% decorations
#' x %>% explicit_guide %>% decorations
#' x %>% explicit_guide(DATE) %>% decorations # limit scope
#' x %$% DATE %>% explicit_guide
explicit_guide.dvec <- function(
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

#' Coerce Guide to Something More Implicit
#'
#' Coerces 'guide' to something more implicit.  Generic, with methods for
#' data.frame.  The key 'guide' generally suggests a guide
#' to interpretation of a data item, such as units, formats, codelists,
#' and encodings.  The idea here is to replace these with 'guide': i.e.,
#' to undo the effects of \code{\link{explicit_guide}}.
#'
#' @param x object of dispatch
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @return see methods
#' @family explicit_guide
#' @md
implicit_guide <- function(x,...)UseMethod('implicit_guide')

#' Coerce Data Frame Guide to Something More Implicit
#'
#' Coerces data.frame guide-like attributes to 'guide'.
#' The attribute 'guide' generally suggests a guide
#' to interpretation of a data item, such as units, formats, codelists,
#' and encodings.  The idea here is to replace these with 'guide':
#' i.e., to undo the effects of \code{\link{explicit_guide.data.frame}}.
#' If guide attribute is still present, the explicit attribute is removed.
#' Otherwise the explicit element is renamed.
#'
#'
#' @param x data.frame
#' @param ... named arguments ignored; un-named arguments limit scope
#' @param collapse numeric: substitute empty codelist if number of levels exceeds this. Set to Inf to ensure levels are always stored explicitly; if zero, empty codelist will always be substituted if codelist elements are un-named and exactly match \code{sort(unique(x))}.
#' @export
#' @keywords internal
#' @importFrom dplyr case_when
#' @importFrom encode encoded
#' @return data.frame
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
#' x %<>% modify(ID, label = 'subject identifier')
#' x %<>% modify(CONC, label = 'concentration', guide = 'ng/mL')
#' x %<>% modify(RACE, label = 'race', guide = list(white = 0, black = 1, asian = 2))
#' x %<>% modify(SEX, label = 'sex', guide = list(female = 0, male = 1))
#' x %<>% modify(DATE, label  = 'date', guide = '%Y-%m-%d')
#' x %>% decorations
#' x %>% explicit_guide %>% decorations
#' x %>% explicit_guide %>% implicit_guide %>% decorations
#' x %>% explicit_guide %>% implicit_guide(DATE) %>% decorations # limit scope
#' x %>% explicit_guide(simplify = FALSE) %>% decorations
#' x %>% explicit_guide(simplify = FALSE) %>% implicit_guide %>% decorations

implicit_guide.data.frame <- function(
    x,
    ...,
    collapse = getOption('yamlet_collapse_codelist', 10)
){
  stopifnot(is.numeric(collapse), length(collapse) == 1)
  
  nms <- selected(x, ...)
  for(nm in nms){
    attr <- attributes(x[[nm]])
    anms <- names(attr)
    anms <- intersect(anms, c('units', 'format', 'codelist', 'encoding'))
    for(anm in anms){
      if('guide' %in% anms){
        attributes(x[[nm]][[anm]]) <- NULL
      } else {
        names(attributes(x[[nm]]))[names(attributes(x[[nm]])) == anm] <- 'guide'
      }
    }
    # we now have a guide attribute for this column, if possible
    guide <- attr(x[[nm]], 'guide')
    if(!is.null(guide)){
      if(is.list(guide)){
        if(length(guide) > 0){
          nms <- names(guide)
          if(is.null(nms)){
            vals <- sort(unique(x[[nm]]))
            have <- unlist(guide)
            if(length(have) == length(guide)){ # i.e. unstructured
              if(length(vals) == length(have)){
                if(all(vals == have)){
                  # guide of x[[nm]] is completely inferred from its values
                  if(length(vals) > collapse) {
                    message(
                      'substituting list() for codelist since ', length(vals), 
                      ' values exceeds collapse = ',
                      collapse, ' (see ?yamlet_options : yamlet_collapse_codelist)'
                    )
                    attr(x[[nm]], 'guide') <- list()
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  x
}



#' Coerce Decorated Vector Guide to Something More Implicit
#'
#' Coerces dvec guide-like attributes to 'guide'.
#' The attribute 'guide' generally suggests a guide
#' to interpretation of a data item, such as units, formats, codelists,
#' and encodings.  The idea here is to replace these with 'guide':
#' i.e., to undo the effects of \code{\link{explicit_guide.dvec}}.
#' If guide attribute is still present, the explicit attribute is removed.
#' Otherwise the explicit element is renamed.
#'
#'
#' @param x dvec
#' @param ... ignored
#' @export
#' @keywords internal
#' @importFrom dplyr case_when
#' @importFrom encode encoded
#' @return dvec
#' @family explicit_guide
#' @family dvec
#' @examples
#' library(magrittr)
#' x <- data.frame(
#'  ID = as_dvec(1),
#'  CONC = as_dvec(1),
#'  RACE = as_dvec(1),
#'  SEX = as_dvec(1),
#'  DATE = as_dvec(1)
#' )
#' x %<>% modify(ID, label = 'subject identifier')
#' x %<>% modify(CONC, label = 'concentration', guide = 'ng/mL')
#' x %<>% modify(RACE, label = 'race', guide = list(white = 0, black = 1, asian = 2))
#' x %<>% modify(SEX, label = 'sex', guide = list(female = 0, male = 1))
#' x %<>% modify(DATE, label  = 'date', guide = '%Y-%m-%d')
#' x %>% decorations
#' x %>% explicit_guide %>% decorations
#' x %>% explicit_guide %>% implicit_guide %>% decorations
#' x %>% explicit_guide %>% implicit_guide(DATE) %>% decorations # limit scope
#' x %>% explicit_guide(simplify = FALSE) %>% decorations
#' x %>% explicit_guide(simplify = FALSE) %>% implicit_guide %>% decorations
#' x %<>% explicit_guide
#' a <- x$DATE
#' str(a)
#' str(a %>% implicit_guide)
implicit_guide.dvec <- function(
    x,
    ...
){
  y <- data.frame(x = x)
  y <- implicit_guide(y)
  y <- y$x
  y
}

