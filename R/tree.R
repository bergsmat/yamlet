#' #' Create a Symbol Tree for Wiki Symbol
#' #'
#' #' Creates a symbol tree for wikisymbol.
#' #' Generic, with method \code{\link{tree.character}}.
#' #' @param x object
#' #' @param ... passed arguments
#' #' @export
#' #' @keywords internal
#' #' @return see methods
#' #' @examples
#' #' tree('1 joule^\\*. ~1 kg m^2./s^2')
#' tree <- function(x, ...)UseMethod('tree')
#'
#' #' Create a Symbol Tree for Wiki Symbol from Character
#' #'
#' #' Creates a symbol tree for wikisymbol from length-one character.
#' #' @param x character
#' #' @param ... ignored arguments
#' #' @export
#' #' @keywords internal
#' #' @return list
#' #' @examples
#' #' tree(character(0))
#' #' tree('')
#' #' tree('a')
#' #' tree('a ')
#' #' tree(' a')
#' #' tree('a^2')
#' #' tree('a\\^2')
#' #' tree('1 joule^\\*. ~1 kg m^2./s^2')
#' #' tree('\\*')
#' tree.character <- function(x, ...){
#'   stopifnot(length(x) <= 1)
#'   # we look for things
#'   # if we find something,
#'   # we create a list with
#'   # this thing as 'token'
#'   # and the tree of
#'   # before and after thus named.
#'   # before and after are only
#'   # created if they have length.
#'   if(!length(x))return(list(token = character(0), before = character(0), after = character(0)))
#'   # handle nests
#'   if(length(this(x, '\\_')))return(bundle(x,'\\_'))
#'   if(length(this(x, '_')))return(bundle(x,'_'))
#'   if(length(this(x, '\\^')))return(bundle(x,'\\^'))
#'   if(length(this(x, '^')))return(bundle(x,'^'))
#'   if(length(this(x, '\\.')))return(bundle(x,'\\.'))
#'   if(length(this(x, '.')))return(bundle(x,'.'))
#'   # handle literals
#'   if(length(this(x, '\\*')))return(bundle(x,'\\*'))
#'   if(length(this(x, '\\s+', fixed = FALSE)))return(bundle(x, '\\s+', fixed = FALSE))
#'   # handle specials
#'   if(length(this(x, '*')))return(bundle(x,'*'))
#'   # handle default
#'   return(list(token = x, before = character(0), after = character(0)))
#' }
#'
#' bundle <- function(x, what, fixed = TRUE)list(
#'   token = this(x, what, fixed = fixed),
#'   before = tree(before(x, what, fixed = fixed)),
#'   after = tree(after(x, what, fixed = fixed))
#' )
#'
#'
#' this <- function(x, what, fixed = TRUE){
#'   at <- regexpr(what, x, fixed = fixed)
#'   if(at == -1) return(character(0))
#'   len <- attr(at, 'match.length')
#'   last <- at + len - 1
#'   ths <- substr(x, start = at, stop = last)
#'   return(ths)
#' }
#' before <- function(x, what, fixed = TRUE){
#'   at <- regexpr(what, x, fixed = fixed)
#'   if(at <= 1) return(character(0))
#'   bef <- substr(x, start = 0, stop = at - 1)
#'   return(bef)
#' }
#' after <- function(x, what, fixed = TRUE){
#'   at <- regexpr(what, x, fixed = fixed)
#'   if(at < 1) return(character(0))
#'   len <- attr(at, 'match.length')
#'   last <- at + len - 1
#'   if(last == nchar(x)) return(character(0))
#'   aft <- substr(x, start = last + 1, stop = nchar(x))
#'   return(aft)
#' }
#' asCharacter <- function(x){
#'   if(is.character(x))return(x)
#'   return(tree_to_plotmath(x))
#' }
#'
#' goodToken <- function(x){
#'   y <- try(silent = TRUE, parse(text = x))
#'   if(inherits(y, 'try-error'))return(FALSE)
#'   TRUE
#' }
#' tokenWrap <- function(x, env){
#'   if(length(asCharacter(env$before))) x <- paste0('*',x)
#'   if(length(asCharacter(env$after)))  x <- paste0(x, '*')
#'   x
#' }
#'
#' #' Coerce Wiki Symbol Tree to Plotmath
#' #'
#' #' Coerces wikisymbol tree to plotmath.
#' #' @param x list with members token, before, and after
#' #' @param ... ignored arguments
#' #' @export
#' #' @keywords internal
#' #' @return character
#' #' @examples
#' #' library(magrittr)
#' #' character(0) %>% tree %>% tree_to_plotmath
#' #' '' %>% tree %>% tree_to_plotmath
#' #' 'a' %>% tree %>% tree_to_plotmath
#' #' 'a ' %>% tree %>% tree_to_plotmath
#' #' ' a' %>% tree %>% tree_to_plotmath
#' #' 'a^2' %>% tree %>% tree_to_plotmath
#' #' 'a^2^3' %>% tree %>% tree_to_plotmath
#' #' 'a^2^3.' %>% tree %>% tree_to_plotmath
#' #' 'a^2.^3' %>% tree %>% tree_to_plotmath #  works!
#' #' 'a^.2' %>% tree %>% tree_to_plotmath # works
#' #' 'a.' %>% tree %>% tree_to_plotmath
#' #' '.a' %>% tree %>% tree_to_plotmath
#' #' 'a^2.' %>% tree %>% tree_to_plotmath
#' #' '^' %>% tree %>% tree_to_plotmath
#' #' '^.' %>% tree %>% tree_to_plotmath
#' #' '.^' %>% tree %>% tree_to_plotmath
#' #' 'a.^2' %>% tree %>% tree_to_plotmath
#' #' '.a^2' %>% tree %>% tree_to_plotmath
#' #' 'a^2^.3' %>% tree %>% tree_to_plotmath
#' #' 'a\\^2' %>% tree %>% tree_to_plotmath
#' #' '\\*' %>% tree %>% tree_to_plotmath
#' #' 'foo' %>% tree %>% tree_to_plotmath
#' #' 'for' %>% tree %>% tree_to_plotmath
#' #' '"' %>% tree %>% tree_to_plotmath
#' #' "'" %>% tree %>% tree_to_plotmath
#' #' '1 joule^\\*. ~1 kg m^2./s^2' %>% tree %>% tree_to_plotmath
#' #' '^\\*' %>% tree %>% tree_to_plotmath
#' tree_to_plotmath <- function(x, ...){
#'   #stopifnot(inherits(x, 'list'))
#'   stopifnot(all(c('token','before','after') %in% names(x)))
#'   token <- x$token
#'   # handle specials
#'   if(identical(x$token, '_')){
#'     token <- character(0) # squelch
#'     x$after <- paste0('[',asCharacter(x$after), ']')
#'     if(!length(asCharacter(x$before))){
#'       x$after <- paste0("''", x$after) # implicit base
#'     }else{
#'       if(!nchar(asCharacter(x$before))){
#'         x$after <- paste0("''", x$after) # implicit base
#'       }
#'     }
#'   }
#'   if(identical(x$token, '^')){
#'     #browser()
#'     token <- character(0) # squelch
#'     x$after <- paste0('^{',asCharacter(x$after), '}')
#'     if(!length(asCharacter(x$before))){
#'       x$after <- paste0("''", x$after) # implicit base
#'     }else{
#'       if(!nchar(asCharacter(x$before))){
#'         x$after <- paste0("''", x$after) # implicit base
#'       }
#'     }
#'   }
#'   if(identical(x$token, '*')){}
#'   if(identical(x$token, '.')){
#'     token <- character(0) # squelch
#'     bef <- asCharacter(x$before)
#'     if(length(bef) && nchar(bef)){
#'       lastChar <- substr(bef, nchar(bef), nchar(bef))
#'        aft <- asCharacter(x$after)
#'        if(length(aft) && nchar(aft)){
#'          nextChar <- substr(aft, 1, 1)
#'          if(!lastChar %in% c('[','{')){
#'             if(!nextChar %in% c('[','{','}',']')){
#'               token <- '*'
#'             }
#'          }
#'       }
#'     }
#'   }
#'
#'   # handle literals
#'   if(identical(x$token, '\\*'))token <- tokenWrap("'*'", x)
#'   if(identical(x$token, '\\.'))token <- tokenWrap("'.'", x)
#'   if(identical(x$token, '\\_'))token <- tokenWrap("'_'", x)
#'   if(identical(x$token, '\\^'))token <- tokenWrap("'^'", x)
#'   if(length(x$token))if(grepl('^\\s+$', x$token)){
#'     token <- paste0("'",x$token,"'")
#'     token <- tokenWrap(token, x)
#'   }
#'   # handle default
#'   if(identical(token, x$token)){ # unmodified from above
#'     if(!goodToken(x$token)){
#'       token <- gsub("'","\\\\'",x$token)
#'       token <- paste0("'", token, "'")
#'     }
#'     token <- tokenWrap(token, x)
#'   }
#'   paste0(asCharacter(x$before), token, asCharacter(x$after))
#' }
#'
#'
