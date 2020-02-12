#' Coerce Character to Axis Label
#'
#' Converts character to axis label, optionally returning an expression
#' intended as \code{\link{plotmath}}.  Respecting units, the intent
#' is to support udunits syntax (\code{\link[units]{as_units}}).
#'
#' If \code{parse} is FALSE, \code{units} (if any) are enclosed
#' in \code{enclose}, and appended to \code{x} with a space
#' separator. If \code{parse} is a function (or its name), it is passed
#' all arguments and the result is returned.
#' if\code{parse} is TRUE, arguments are processed as follows:
#'
#' * label is converted using \code{\link[spork]{as_wikisymbol}} and \code{\link[spork]{as_plotmath}},
#' * spaces in the label are then replaced with plotmath space,
#' #' * units if present are converted similarly,
#' #' * units will be enclosed using \code{enclose} values with \code{group()},
#' #' * label and units(if any) will be combined using \code{paste}, and
#' #' * the result is returned as an expression.
#'
#' #' @param x character
#' #' @param units value to use for units
#' #' @param enclose length-two character for enclosing unit
#' #' @param parse whether to convert to expression, or function to do the same
#' #' @param ... passed arguments
#' #' @return character, or expression if parse is TRUE.
#' #' @export
#' #' @keywords internal
#' #' @family lab
#' as_lab.character <- function(
#'   x,
#'   units = character(0),
#'   enclose = getOption('yamlet_enclose', default = c('(',')')),
#'   parse = getOption('yamlet_label_parse', FALSE),
#'   ...
#' ){
#'   # u <- paste0(enclose[[1]], u, enclose[[2]])
#'   # out <- paste(out, u)
#'   stopifnot(length(x) == 1)
#'   stopifnot(length(units) <= 1)
#'   stopifnot(length(enclose) == 2)
#'   if(!is.logical(parse)){
#'     fun <- try(silent = TRUE, match.fun(parse))
#'     if(inherits(fun, 'try-error')){
#'       warning('could not coerce parse to function')
#'       parse <- FALSE
#'     }else{
#'       return(
#'         fun(parse)(x = x, units = units, enclose = enclose, ...)
#'       )
#'     }
#'   }
#'   # now parse is logical
#'   stopifnot(length(parse) == 1)
#'   if(!parse){
#'     return(
#'       paste(
#'         x,
#'         paste0(enclose[[1]], units, enclose[[2]])
#'       )
#'     )
#'   }
#'   # now parse is true, no function passed
#'   label <- as_plotmath(as_wikisymbol(x))
#'   label <- gsub(' ','~',label)
#'   if(length(units)){
#'     units <- as_plotmath(as_wikisymbol(units))
#'     if(nchar(label)){
#'       units <-paste0('~group("', enclose[[1]],'",', units, ',"', enclose[[2]], '"',')')
#'     }
#'     label <- paste0(label,'~', units)
#'   }
#'   res <- parse(text = label)
#'   res
#' }
#'
#' #' Coerce to Axis Label
#' #'
#' #' Converts to axis label. Generic, with method \code{\link{as_lab.list}}.
#' #' @param x object
#' #' @param ... passed to other methods
#' #' @return character, or expression if parse is TRUE
#' #' @export
#' #' @keywords internal
#' #' @family lab
#' as_lab <- function(x,...)UseMethod('as_lab')
#'
#' #' Coerce List to Axis Label
#' #'
#' #' Coerces list to axis label. Accepts a default label
#' #' and returns that if nothing better can be done.
#' #' If the attribute list has one named 'label', it
#' #' is chosen as a substitute.  But if that attribute
#' #' is itself a list of values, an attempt is made
#' #' to identify a single relevant value by treating
#' #' the value names as conditions to evaluate on
#' #' the supplied data. If a suitable value is
#' #' found, it is chosen as a substitute.  See
#' #' \code{\link{singularity}} for search logic.
#' #'
#' #' If \code{parse} is TRUE, \code{\link{as_lab.character}}
#' #' is called by default; supply your own function
#' #' as the value of \code{parse} for finer control.
#' #'
#' #' @param x list, such as returned by \code{\link{attributes}}.
#' #' @param default a value to return by default
#' #' @param collapse character: separator for collapsing multi-line units
#' #' @param enclose length-two character for enclosing unit
#' #' @param data data.frame for resolving competing named values
#' #' @param parse whether to convert to expression, or function to do the same
#' #' @param ... passed to other methods
#' #' @return length-one character
#' #' @export
#' #' @family lab
#' #' @examples
#' #' meta <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#' #' x <- decorate(meta)
#' #' as_lab(attributes(x$time), 'time', enclose = c('[',']'))
#' #' as_lab(attributes(x$time), 'time', enclose = c('[ ',' ]'))
#' as_lab.list <- function(
#'   x,
#'   default,
#'   collapse = '\n',
#'   enclose = getOption('yamlet_enclose', default = c('(',')')),
#'   parse = getOption('yamlet_label_parse', FALSE),
#'   data,
#'   ...
#' ){
#'   stopifnot(length(default) == 1, is.character(default))
#'   stopifnot(length(enclose) == 2, is.character(enclose))
#'   out <- default
#'   if('label' %in% names(x)){
#'     candidate <- x$label
#'     if(length(candidate) == 1){
#'       out <- unlist(candidate)
#'     }else{
#'       # multiple labels
#'       dex <- singularity(
#'         names(candidate),
#'         data
#'       )
#'       if(!is.na(dex)){
#'         if(dex > 0){
#'           out <- candidate[[dex]]
#'         }
#'       }
#'     }
#'   }
#'   u <- character(0)
#'   if('units' %in% names(x)) u <- x$units
#'   if('unit' %in% names(x)) u <- x$unit
#'   if('guide' %in% names(x)) u <- x$guide # such as encoding or unit
#'   # if(length(x$guide) == 1)
#'   #if(length(u) > 1) u <- paste(u, collapse = collapse)
#'   if(length(u) > 1){ # named levels? or conditional units?
#'     dex <- singularity(names(u), data)
#'     if(!is.na(dex)){
#'       if(dex > 0){
#'         u <- u[[dex]]
#'       }else{
#'         u <- character(0)
#'       }
#'     }else{
#'       u <- character(0)
#'     }
#'   }
#'   if(length(u)) {
#'     # at this point, u should be length-one character
#'     # just in case, we can collapse it to ensure singularity
#'     if(length(u) > 1){
#'       u <- paste(u, collapse = collapse)
#'     }
#'   }
#'   # pass to as_lab.character
#'   res <- as_lab(x = out, units = u, enclose = enclose, parse = parse, ... )
#'   res
#' }
# print.ag <- function(x, labeller = getOption('yamlet_labeller', default = as_lab), ...){
#   fun <- match.fun(labeller)
#   for(i in seq_along(x$labels)){           # x (gg object) stores names of used columns as $labels
#     lab <- x$labels[[i]]                   # deal with one label
#
#     if(lab %in% names(x$data)){            # if this is just a bare column name
#       attr <- attributes(x$data[[lab]])    # retrieve the attributes
#       if(!is.null(attr)){
#         val <- fun(x = attr, default = lab, data = x$data, ...)
#         x$labels[[i]] <- val               # replace default label with one from labeller
#       }
#       # while we are here, we should
#       #promote lab to factor if appropriate
#       guide <- attr$guide
#       table <- x$data[[lab]]
#       if(length(guide) > 1){
#         if(!isConditional(guide, x$data)){
#           if(!is.factor(table)){ # is.vector returns false if x has non-name attributes
#             if(isLevels(guide, table)){
#               labels <- as.character(guide)
#               if(length(names(guide))){
#                 if(!any(names(guide) == '')){
#                   labels <- names(guide)
#                 }
#               }
#               try(
#                 x$data[[lab]] <- factor(
#                   x$data[[lab]],
#                   levels = as.character(attr$guide),
#                   labels = labels
#                 )
#               )
#             }
#           }
#         }
#       }
#     }
#   }
#   NextMethod()
# }
