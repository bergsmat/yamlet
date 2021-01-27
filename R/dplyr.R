#' Coerce to Decorated
#'
#' Coerces to decorated by subclassing.
#' @param x object of dispatch
#' @param ... passed arguments
#' @family dplyr
#' @export
#' @keywords internal
as_decorated <- function(x, ...){
  class(x) <- union('decorated', class(x))
  x
}

#' Slice Decorated
#'
#' Preserves class when slicing decorated.
#' @importFrom dplyr slice
#' @export
#' @keywords internal
#' @family dplyr
#' @inheritParams dplyr::slice
slice.decorated <- function(.data, ..., .preserve = FALSE){
  as_decorated(NextMethod())
}

#' Filter Decorated
#'
#' Preserves class when filtering decorated.
#' @param .data see \code{\link[dplyr]{filter}}
#' @param ... see \code{\link[dplyr]{filter}}
#' @param preserve see \code{\link[dplyr]{filter}}
#' @importFrom dplyr filter
#' @export
#' @keywords internal
#' @family dplyr
filter.decorated <- function(.data, ..., .preserve = FALSE){
  as_decorated(NextMethod())
}

#' Select Decorated
#'
#' Preserves class when selecting decorated.
#' @importFrom dplyr select
#' @export
#' @keywords internal
#' @family dplyr
#' @inheritParams dplyr::select
select.decorated <- function(.data, ...){
  as_decorated(NextMethod())
}
#' Arrange Decorated
#'
#' Preserves class when arranging decorated.
#' @importFrom dplyr arrange
#' @export
#' @keywords internal
#' @family dplyr
#' @inheritParams dplyr::arrange
arrange.decorated <- function(.data, ...){
  as_decorated(NextMethod())
}

#' Group_by Decorated
#'
#' Preserves class when grouping decorated.
#' @importFrom dplyr group_by
#' @importFrom dplyr group_by_drop_default
#' @export
#' @keywords internal
#' @family dplyr
#' @inheritParams dplyr::group_by
group_by.decorated <- function(
  .data, ...,
  add = FALSE, .drop = group_by_drop_default(.data)){
  as_decorated(NextMethod())
}

#' Mutate Decorated
#'
#' Preserves class when mutating decorated.
#' @importFrom dplyr mutate
#' @export
#' @keywords internal
#' @family dplyr
#' @inheritParams dplyr::mutate
mutate.decorated <- function(.data, ...){
  as_decorated(NextMethod())
}

#' Summarize Decorated
#'
#' Preserves class when summarizing decorated.
#' @importFrom dplyr summarize
#' @export
#' @keywords internal
#' @family dplyr
#' @inheritParams dplyr::summarize
summarize.decorated <- function(.data, ...){
  as_decorated(NextMethod())
}
#' Summarise Decorated
#'
#' Preserves class when summarising decorated.
#' @importFrom dplyr summarise
#' @export
#' @keywords internal
#' @family dplyr
#' @inheritParams dplyr::summarise
summarise.decorated <- function(.data, ...){
  as_decorated(NextMethod())
}

#' Semi_join Decorated
#'
#' Preserves class when joining decorated.
#' @importFrom dplyr semi_join
#' @export
#' @keywords internal
#' @family dplyr
#' @inheritParams dplyr::semi_join
semi_join.decorated <- function(x, y, by = NULL, copy = FALSE, ...){
  as_decorated(NextMethod())
}

#' Anti_join Decorated
#'
#' Preserves class when joining decorated.
#' @importFrom dplyr anti_join
#' @export
#' @keywords internal
#' @family dplyr
#' @inheritParams dplyr::anti_join
anti_join.decorated <- function(x, y, by = NULL, copy = FALSE, ...){
  as_decorated(NextMethod())
}

#' Full_join Decorated
#'
#' Preserves class when joining decorated.
#' @importFrom dplyr full_join
#' @export
#' @keywords internal
#' @family dplyr
#' @inheritParams dplyr::full_join
full_join.decorated <- function(
  x, y, by = NULL,
  copy = FALSE, suffix = c(".x", ".y"), ...){
  as_decorated(NextMethod())
}


#' Inner_join Decorated
#'
#' Preserves class when joining decorated.
#' @importFrom dplyr inner_join
#' @export
#' @keywords internal
#' @family dplyr
#' @inheritParams dplyr::inner_join
inner_join.decorated <- function(
  x, y, by = NULL,
  copy = FALSE, suffix = c(".x", ".y"), ...){
  as_decorated(NextMethod())
}

#' Left_join Decorated
#'
#' Preserves class when joining decorated.
#' @importFrom dplyr left_join
#' @export
#' @keywords internal
#' @family dplyr
#' @inheritParams dplyr::left_join
left_join.decorated <- function(x, y, by = NULL,
                                copy = FALSE, suffix = c(".x", ".y"), ...){
  as_decorated(NextMethod())
}

#' Right_join Decorated
#'
#' Preserves class when joining decorated.
#' @importFrom dplyr right_join
#' @export
#' @keywords internal
#' @family dplyr
#' @inheritParams dplyr::right_join
right_join.decorated <- function(x, y, by = NULL,
  copy = FALSE, suffix = c(".x", ".y"), ...){
  as_decorated(NextMethod())
}

#' @export
#' @keywords internal
dplyr::filter
