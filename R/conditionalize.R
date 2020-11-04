globalVariables('token')
#' Conditionalize Attributes
#'
#' Conditionalizes attributes of something.
#' Generic, with method for data.frame.
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family conditionalize
#' @return see methods
#' @examples
#' example(conditionalize.data.frame)
conditionalize <- function(x, ...)UseMethod('conditionalize')


#' Conditionalize Attributes of Data Frame
#'
#' Conditionalizes attributes of data.frame.
#' Creates a conditional \code{attribute} definition
#' for \code{column} by mapping \code{value} to
#' \code{test}. Only considers records where
#' both \code{test} and \code{value} are defined,
#' and gives an error if there is not one-to-one mapping.
#' Can be used with write methods as an alternative
#' to hand-coding conditional metadata.
#'
#' If the test column is character, individual
#' elements should not contain both single and
#' double quotes. For the conditional expressions,
#' these values will be single-quoted by default,
#' or double-quoted if they contain single quotes.
#'
#' @param x data.frame
#' @param column unquoted name of column to conditionalize
#' @param attribute unquoted name of attribute to create for column
#' @param test unquoted name of column to test
#' @param value unquoted name of column supplying attribute value
#' @param ... ignored arguments
#' @importFrom rlang ensym
#' @importFrom rlang :=
#' @importFrom dplyr mutate
#' @importFrom dplyr distinct
#' @export
#' @keywords internal
#' @family conditionalize
#' @return class 'decorated' 'data.frame'
#' @examples
#' library(magrittr)
#' library(dplyr)
#' library(csv)
#' file <- system.file(package = 'yamlet', 'extdata','phenobarb.csv')
#' x <- as.csv(file)
#' head(x,3)
#'
#' # suppose we have an event label stored as a column:
#'
#' x %<>% mutate(evid = ifelse(
#'   event == 'dose',
#'   'dose of drug administered',
#'   'serum phenobarbital concentration'
#'  )
#' )
#'
#' # We can define a conditional label for 'value'
#' # by mapping evid to event:
#'
#' x %<>% conditionalize(value, label, event, evid)
#'
#' x %>% as_yamlet
#' x %>% write_yamlet
#'
conditionalize.data.frame <- function(x, column, attribute, test, value, ...){
  col <- as.character(ensym(column))
  atr <- as.character(ensym(attribute))
  tst <- ensym(test)
  val <- ensym(value)
  y <- filter(x, !is.na(!!tst), !is.na(!!val))
  map <- distinct(y, !!tst, !!val)
  if(
    ( nrow(map) ) !=
    (nrow(distinct(y, !!tst)))
  )stop("'", as.character(val), "' not cleanly mapped to defined '", as.character(tst),"'")
  #map <- mutate(map, !!tst := as.character(!!tst))
  if(is.factor(map[[as.character(tst)]])){
    map[[as.character(tst)]] <- as.character(map[[as.character(tst)]])
  }
  m <- map[[as.character(tst)]]
  if(is.character(m)){
    if(any(grepl("'", m) & grepl('"', m))){
      stop(as.character(tst), ' has mixed single and double quotes')
    }
    map <- mutate(map, token = ifelse(grepl("'", !!tst), '"', "'"))
    map <- mutate(map, !!tst := paste0(token, !!tst, token))
  }
  map <- mutate(map, !!tst := paste(tst, '==', !!tst))
  out <- as.list(map[[as.character(val)]])
  names(out) <- map[[as.character(tst)]]
  attr(x[[col]], atr) <- out
  x
}
