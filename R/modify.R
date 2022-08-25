#' Modify Attributes of Indicated Components
#'
#' Modifies the attributes of indicated components.
#' Generic, with method \code{\link{modify.default}}.
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @return see methods
#' @family modify
#' @examples
#' example(modify.default)
modify <- function(x, ...)UseMethod('modify')

#' Modify Attributes of Indicated Components by Default
#'
#' Modifies the attributes of each indicated element
#' (all elements by default).  Tries to assign the value of an expression
#' to the supplied label, with existing attributes
#' and the object itself (.) available as arguments.
#' Gives a warning if the supplied label is considered reserved.
#' Intends to support anything with one or more non-empty names.
#'
#' The name of the component itself is available during assignments as
#' attribute 'name' (any pre-existing attribute 'name' is temporarily masked).
#' After all assignments are complete, the value of 'name' is enforced at the object level.
#' Thus, \code{modify} expressions can modify component names.
#'
#' As currently implemented, the expression is evaluated by
#' \code{\link[rlang]{eval_tidy}}, with attributes supplied as
#' the \code{data} argument.  Thus, names in the expression
#' may be disambiguated, e.g. with \code{.data}.  See examples.
#'
#' @param x object
#' @param ... indicated columns, or name-value pairs
#' @param .reserved reserved labels that warn on assignment
#' @export
#' @importFrom rlang f_rhs eval_tidy quo_set_env quos new_data_mask
#' @return same class as x
#' @family modify
#' @family interface
#' @examples
#' library(magrittr)
#' library(dplyr)
#' file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' x <- decorate(file)
#'
#' # modify selected columns
#' x %<>% modify(title = paste(label, '(', guide, ')'), time)
#' x %>% select(time, conc) %>% decorations
#'
#' # modify (almost) all columns
#' x %<>% modify(title = paste(label, '(', guide, ')'), -Subject)
#' x %>% select(time, conc) %>% decorations
#'
#' # use column itself
#' x %<>% modify(`defined values` = sum(!is.na(.)))
#' x %>% select(time) %>% decorations
#'
#' # rename column
#' x %<>% modify(time, name = label)
#' names(x)
#'
#' # warn if assignment fails
#' \dontrun{
#' \donttest{
#' x %<>% modify(title = foo, time)
#' }}
#' 
#' # support lists
#' list(a = 1, b = 1:10, c = letters) %>%
#' modify(length = length(.), b:c)
#'
#'x %<>% select(Subject) %>% modify(label = NULL, `defined values` = NULL)
#'
#' # distinguish data and environment
#' location <- 'environment'
#' x %>% modify(where = location) %>% decorations
#' x %>% modify(where = .env$location) %>% decorations
#' \dontrun{
#' \donttest{
#' x%>% modify(where = .data$location) %>% decorations
#' }}
#' x %>% modify(location = 'attributes', where = location) %>% decorations
#' x %>% modify(location = 'attributes', where = .data$location) %>% decorations
#'
modify.default <- function(
  x,
  ...,
  .reserved = getOption(
    'yamlet_modify_reserved',
    c('class','levels','labels','names')
  )
){
  stopifnot(is.character(.reserved))
  vars <- selected(x, ...)
  mods <- quos(...)
  mods <- mods[names(mods) != '']
  reserved <- intersect(names(mods), .reserved)
  if(length(reserved))warning('reserved: ', paste(reserved, collapse = ', '))
  for(var in vars){
    was <- attr(x[[var]], 'name', exact = TRUE)
    attr(x[[var]], 'name') <- var
    for(mod in names(mods)){
     expr <- mods[[mod]] # a quosure
     attr <- attributes(x[[var]])
     attr <- attr[names(attr) != ''] # see ?list2env
     attr <- c(attr, list(. = x[[var]]))
     # env <- list2env(attr)
     # expr <- rlang::quo_set_env(quo = expr, env = env)
     # mask <- new_data_mask(env)
      tryCatch(
        attr(x[[var]], mod) <- rlang::eval_tidy(expr, data = attr),
        error = function(e)warning(var, ': ', e)
      )
    }
    i <- match(var, names(x), nomatch = 0) # singular
    stopifnot(i > 0)
    names(x)[[i]] <- attr(x[[i]],'name')
    attr(x[[i]], 'name') <- was
  }
  x
}

