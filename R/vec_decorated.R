# https://vctrs.r-lib.org/reference/howto-faq-coercion-data-frame.html

#' Determine If Tibble is Present
#' 
#' Determines if a tibble is present.
#' True if either x or y inherits tbl_df.
#' @export
#' @keywords internal
#' @return boolean
#' @param x object
#' @param y object
#' @param ... ignored
tibbled <- function(x, y, ...)FALSE
# {
#   left <- inherits(x, 'tbl_df')
#   right <- inherits(y, 'tbl_df')
#   left | right
# }


#' Coerce Common Type to Decorated Tibble
#' 
#' Coerces common type to tibble.  Wrapper for \code{\link[vctrs]{tib_ptype2}}.
#' @export
#' @keywords internal
#' @return decorated tbl_df
#' @param x subclass of data.frame
#' @param y subclass of data.frame
#' @param ... passed arguments
#' @importFrom vctrs tib_ptype2

td_ptype2 <- function(x, y, ...) {
  as_decorated(tib_ptype2(x, y, ...))
}


#' Coerce Tibble to Decorated Tibble
#' 
#' Coerces tibble to decorated tibble.  Wrapper for \code{\link[vctrs]{tib_cast}}.
#' @export
#' @keywords internal
#' @return decorated tbl_df
#' @param x subclass of data.frame
#' @param to subclass of data.frame
#' @param ... passed arguments
#' @importFrom vctrs tib_cast

td_cast <- function(x, to, ...) {
  as_decorated(tib_cast(x, to, ...))
}

#' Coerce Common Type to Decorated
#' 
#' Coerces common type to decorated.  Wrapper for \code{\link[vctrs]{df_ptype2}}.
#' @export
#' @keywords internal
#' @return decorated
#' @param x subclass of data.frame
#' @param y subclass of data.frame
#' @param ... passed arguments
#' @importFrom vctrs df_ptype2

dd_ptype2 <- function(x, y, ...) {
  as_decorated(df_ptype2(x, y, ...))
}


#' Coerce Data.frame to Decorated
#' 
#' Coerces data.frame to decorated.  Wrapper for \code{\link[vctrs]{df_cast}}.
#' @export
#' @keywords internal
#' @return decorated
#' @param x subclass of data.frame
#' @param to subclass of data.frame
#' @param ... passed arguments
#' @importFrom vctrs df_cast

dd_cast <- function(x, to, ...) {
  as_decorated(df_cast(x, to, ...))
}

#' Determine Common Type for Decorated
#' 
#' Determines common type for decorated.
#' @return decorated
#' @param x decorated
#' @param y decorated
#' @param ... passed arguments
#' @export
#' @keywords internal
vec_ptype2.decorated.decorated <- function(x, y, ...) {
  if(tibbled(x, y, ...)) return(td_ptype2(x, y, ...))
  dd_ptype2(x, y, ...)
}

#' Determine Common Type for Decorated and Data.frame
#' 
#' Determines common type for decorated and data.frame.
#' @return decorated
#' @param x decorated
#' @param y data.frame
#' @param ... passed arguments
#' @export
#' @keywords internal
vec_ptype2.decorated.data.frame <- function(x, y, ...) {
  if(tibbled(x, y, ...)) return(td_ptype2(x, y, ...))
  dd_ptype2(x, y, ...)
}
#' Determine Common Type for Data.frame and Decorated
#' 
#' Determines common type for data.frame and decorated.
#' @return decorated
#' @param x data.frame
#' @param y decorated
#' @param ... passed arguments
#' @export
#' @keywords internal
vec_ptype2.data.frame.decorated <- function(x, y, ...) {
  if(tibbled(x, y, ...)) return(td_ptype2(x, y, ...))
  dd_ptype2(x, y, ...)
}

#' Cast to Decorated from Decorated
#' 
#' Casts to decorated from decorated.
#' @export
#' @return decorated
#' @param x decorated
#' @param to decorated
#' @param ... passed arguments
#' @keywords internal
vec_cast.decorated.decorated <- function(x, to, ...) {
  if(tibbled(x, y, ...)) return(td_cast(x, to, ...))
  dd_cast(x, to, ...)
}
#' Cast to Decorated from Data.frame
#' 
#' Casts to decorated from data.frame.
#' @export
#' @return decorated
#' @param x data.frame
#' @param to decorated
#' @param ... passed arguments
#' @keywords internal
vec_cast.decorated.data.frame <- function(x, to, ...) {
  # `x` is a data.frame to be converted to a decorated
  if(tibbled(x, y, ...)) return(td_cast(x, to, ...))
  dd_cast(x, to, ...)
}
#' Cast to Data.frame from Decorated
#' 
#' Casts to data.frame from decorated.
#' @export
#' @return data.frame
#' @param x decorated
#' @param to data.frame
#' @param ... passed arguments
#' @keywords internal
vec_cast.data.frame.decorated <- function(x, to, ...) {
  # `x` is a decorated to be converted to a data.frame
  df_cast(x, to, ...)
}







