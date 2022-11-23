# https://vctrs.r-lib.org/reference/howto-faq-coercion-data-frame.html



# Determine Common Type for Decorated
# 
# Determines common type for decorated.
# @return decorated
# @param x decorated
# @param y decorated
# @param ... passed arguments
# @export
# @keywords internal
# vec_ptype2.decorated.decorated <- function(x, y, ...) {
#   dd_ptype2(x, y, ...)
# }

#  Determine Common Type for Decorated and Tibble
#  
#  Determines common type for decorated and tibble.
#  @return decorated tbl_df
#  @param x decorated
#  @param y tbl_df
#  @param ... passed arguments
#  @export
#  @keywords internal
# vec_ptype2.decorated.tbl_df <- function(x, y, ...) {
#   td_ptype2(x, y, ...)
# }
#  Determine Common Type for Tibble and Decorated
#  
#  Determines common type for tibble and decorated.
#  @return decorated tbl_df
#  @param x tbl_df
#  @param y decorated
#  @param ... passed arguments
#  @export
#  @keywords internal
# vec_ptype2.tbl_df.decorated <- function(x, y, ...) {
#   td_ptype2(x, y, ...)
# }

# Cast to Decorated from Decorated
# 
# Casts to decorated from decorated.
# @export
# @return decorated
# @param x decorated
# @param to decorated
# @param ... passed arguments
# @keywords internal
# vec_cast.decorated.decorated <- function(x, to, ...) {
#   dd_cast(x, to, ...)
# }

#  Cast to Decorated Tibble from Tibble
#  
#  Casts to decorated tibble from tibble.
#  @export
#  @return decorated tbl_df
#  @param x tbl_df
#  @param to decorated
#  @param ... passed arguments
#  @keywords internal
# vec_cast.decorated.tbl_df <- function(x, to, ...) {
#   # `x` is a tbl_df to be converted to a decorated
#   dd_cast(x, to, ...)
# }
#  Cast to Tibble from Decorated
#  
#  Casts to tibble from decorated.
#  @export
#  @return decorated tbl_df
#  @param x decorated
#  @param to tbl_df
#  @param ... passed arguments
#  @keywords internal
# vec_cast.tbl_df.decorated <- function(x, to, ...) {
#   # `x` is a decorated to be converted to tbl_df
#   td_cast(x, to, ...)
# }







