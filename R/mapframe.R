globalVariables(c('data', 'aesthetic'))
#' Coerce Mapping to Dataframe
#' 
#' Coerces mapping to 'data.frame'.
#' Generic, with methods \code{\link{mapframe.default}}
#' and \code{\link{mapframe.ggplot}}.
#' 
#' @param x object of dispatch
#' @param ... ignored
#' @return see methods
#' @export
#' @importFrom ggplot2 ggplot_build
#' @importFrom rlang as_label
#' @importFrom dplyr bind_rows filter
#' @keywords internal
#' @family decorated_ggplot
mapframe <- function(x, ...)UseMethod('mapframe')

#' Coerce ggplot Mapping to Dataframe
#' 
#' Coerces ggplot mapping to 'data.frame'.
#' 
#' * 'aesthetic' captures each aesthetic label as a string.
#' * 'map' captures each map (typ. a column name) as a string.
#' 
#' @param x mapping
#' @param ... ignored
#' @return data.frame
#' @export
#' @keywords internal
#' @family decorated_ggplot
#' @examples
#' library(ggplot2)
#' mapframe(ggplot(mapping = aes(conc, time, shape = factor(dose)))$mapping)
mapframe.default <- function(x, ...){
  cols <- sapply(x, rlang::as_label)
  map <- data.frame(aesthetic = names(cols), map = cols)
  rownames(map) <- NULL
  map
}

#' Coerce ggplot Mappings to Dataframe
#' 
#' Coerces ggplot mappings to 'data.frame'.
#' 
#' * 'aesthetic' captures each aesthetic label as a string.
#' * 'map' captures each map (typ. a column name) as a string.
#' * 'layer' is zero for the default mapping, else the layer number.
#' * 'data' is NA for default data if missing. Else zero for x$data,
#'     zero for layers with no data, and layer number for layers with data.
#' 
#' @param x ggplot
#' @param ... ignored
#' @return data.frame
#' @export
#' @keywords internal
#' @family decorated_ggplot
#' @examples
#' library(ggplot2)
#' mapframe(ggplot(mapping = aes(conc, time, shape = factor(dose))))
mapframe.ggplot <- function(x, ...){
  map <- mapframe(x$mapping)
  if(nrow(map)){
    map$layer <- 0
    map$data <- if(!length(x$data))NA else 0
  }
  for(i in seq_along(x$layers)){
    new <- mapframe(x$layers[[i]]$mapping)
    if(nrow(new)){
      new$layer <- i
      if(!length(x$layers[[i]]$data)){
        new$data <- if(!length(x$data))NA else 0
      } else {
        new$data <- i
      }
      map <- bind_rows(map, new)
    }
  }
  if(!nrow(map)) map <- data.frame(
    aesthetic = character(0), 
    map = character(0),
    layer = integer(0), 
    data = integer(0)
  )
  map
}

#' Retrieve Data Context
#' 
#' Retrieves data context as 'data.frame'.
#' Generic, with methods \code{\link{data_context.ggplot}}.
#' 
#' @param x object of dispatch
#' @param ... ignored
#' @return data.frame
#' @export
#' @keywords internal
#' @family decorated_ggplot
data_context <- function(x, ...)UseMethod('data_context')


#' Retrieve Data Context of ggplot
#' 
#' Retrieves data context of ggplot as 'data.frame'.
#' calls \code{\link{mapframe.ggplot}}, 
#' and limits this to rows where 'data' is not NA
#' and 'aesthetic' is not duplicated.
#' I.e., for each aesthetic, this function 
#' returns the first mapping that has (at least default)
#' data.  Such mappings should be what ggplot uses
#' to train the corresponding scales.
#' 
#' @param x ggplot
#' @param ... ignored
#' @return data.frame
#' @export
#' @keywords internal
#' @family decorated_ggplot
data_context.ggplot <- function(x, ...){
  map <- mapframe(x)
  if(!nrow(map))return(map)
  # scales to be trained on first layer that 
  # defines the aesthetic and has (at least default) data
  map <- filter(map, !is.na(data))
  map <- filter(map, !duplicated(aesthetic))
  map
}
