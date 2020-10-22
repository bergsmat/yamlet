#' Modify Attributes of Indicated Components
#'
#' Modifies the attributes of indicated components.
#' Generic, with default method.
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @return see methods
#' @family modify
#' @examples
#' example(modify.data.frame)
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
#' @param x object
#' @param ... indicated columns, or name-value pairs
#' @param .reserved reserved labels that warn on assignment
#' @export
#' @importFrom rlang f_rhs eval_tidy quo_set_env quos
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
#' x %>% select(time, conc) %>% as_yamlet
#'
#' # modify (almost) all columns
#' x %<>% modify(title = paste(label, '(', guide, ')'), -Subject)
#' x %>% select(time, conc) %>% as_yamlet
#'
#' # use column itself
#' x %<>% modify(`defined values` = sum(!is.na(.)))
#' x %>% select(time) %>% as_yamlet
#'
#' # warn if assignment fails
#' \dontrun{
#' \donttest{
#' x %<>% modify(title = foo, time)
#'}}
#' # support lists
#' list(a = 1, b = 1:10, c = letters) %>%
#' modify(length = length(.), b:c)
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
  args <- quos(...)
  # args <- lapply(args, rlang::f_rhs)
  vars <- args[names(args) == ""]
  mods <- args[names(args) != ""]
  # vars <- lapply(vars, rlang::f_rhs)
  # vars <- sapply(vars, as.character)
  y <- names(x) # should work if x has names
  y <- y[y != ''] # ignore empty names
  d <- lapply(y, function(i)character())
  names(d) <- y # reuse names
  d <- data.frame(d)# dummy data.frame for dplyr
  vars <- d %>% select(!!!vars) %>% names
  if(length(vars) == 0) vars <- names(x)
  reserved <- intersect(names(mods), .reserved)
#  missing <- setdiff(vars, names(x))
  vars <- intersect(vars, names(x))
  if(length(reserved))warning('reserved: ', paste(reserved, collapse = ', '))
#  if(length(missing))warning('missing: ', paste(missing, collapse = ', '))
  for(var in vars){
   for(mod in names(mods)){
     expr <- mods[[mod]] # a quosure
     attr <- attributes(x[[var]])
     attr <- attr[names(attr) != ''] # see ?list2env
     attr <- c(attr, list(. = x[[var]]))
     env <- list2env(attr)
     expr <- rlang::quo_set_env(quo = expr, env = env)
      tryCatch(
        attr(x[[var]], mod) <- rlang::eval_tidy(expr),
        error = function(e)warning(var, ': ', e)
      )
    }
  }
  x
}

