#' Parse Wiki Symbol
#'
#' Parses wikisymbol.  Converts length-one character
#' to vector of tokens.  Explicit tokens include
#' \code{*._^} and any of these escaped with
#' backslash, e.g. \code{\\*}. Consecutive
#' whitespace characters are a single token.
#' Any string of characters delimited by
#' one or more of the above is implicitly
#' a token as well.
#'
#' @param x length-one character using wikisymbol syntax
#' @param ... ignored arguments
#' @export
#' @keywords internal
#' @return character
#' @family wikisymbol
#' @examples
#' wikitoken('Omega joule^\\*. ~1 kg m^2./s^2')

wikitoken <- function(x, ...){
  x <- as.character(x)
  stopifnot(length(x) <= 1)
  if(length(x) == 0) return(character(0))
  if(x == '')return('')
  input <- x
  output <- character(0)
  explicit <- c(
    '\\s+',
    '[*]','[.]','[_]','\\^',
    '[\\][*]','[\\][.]','[\\][_]','[\\]\\^'
  )
  while(nchar(input)){
    m <- sapply(explicit, function(pattern)position(input, pattern))
    if(max(m) == -1)return(c(output, input))
    m <- m[m != -1]
    m <- m[m == min(m)]
    stopifnot(length(m) == 1)
    p <- names(m)
    output <- c(
      output,
      before(input, p),
      this(input, p)
    )
    input <- after(input, p)
  }
  return(output)
}

position <- function(x, what, fixed = FALSE)as.integer(regexpr(what, x, fixed = fixed))

this <- function(x, what, fixed = FALSE){
  at <- regexpr(what, x, fixed = fixed)
  if(at == -1) return(character(0))
  len <- attr(at, 'match.length')
  last <- at + len - 1
  ths <- substr(x, start = at, stop = last)
  return(ths)
}
before <- function(x, what, fixed = FALSE){
  at <- regexpr(what, x, fixed = fixed)
  if(at <= 1) return(character(0))
  bef <- substr(x, start = 0, stop = at - 1)
  return(bef)
}
after <- function(x, what, fixed = FALSE){
  at <- regexpr(what, x, fixed = fixed)
  if(at < 1) return(character(0))
  len <- attr(at, 'match.length')
  last <- at + len - 1
  if(last == nchar(x)) return(character(0))
  aft <- substr(x, start = last + 1, stop = nchar(x))
  return(aft)
}
