# as of 0.9.4, printing/plotting ggready redirects to printing decorated.
# Create a New ggplot for a GGready Data Frame
#
# Creates a new ggplot object for a ggready data.frame.
# This is the ggplot() method for class 'ggready';
# it tries to implement automatic labels and units in axes and legends
# in association with \code{\link{print.ggready_ggplot}}.
# This approach is deprecated in favor of \code{\link{ggplot.decorated}}.
#
# @param data data.frame or similar
# @param ... passed to \code{\link[ggplot2]{ggplot}}
# @return return value like \code{\link[ggplot2]{ggplot}}
# @export
# @importFrom ggplot2 ggplot
# @family ggready
# @keywords internal
# @examples
# example(ggready)

# ggplot.ggready <- function(data, ...){
#   class(data) <- setdiff(class(data), 'ggready')
#   p <- ggplot(data = data, ...)
#   class(p) <- c('ggready_ggplot',class(p))
#   p
# }

# Print Automatic Labels and Units for ggplot
#
# Prints automatic labels and units for ggplot.
# Substitutes column label, if present, for default.
#
# @param x class 'ggready_ggplot' from \code{\link{ggplot.ggready}}
# @param ... passed arguments
# @return see \code{\link[ggplot2]{print.ggplot}}
# @export
# @keywords internal
# @family ggready
# @examples
# example(ggready)

# print.ggready_ggplot <- function(x, ...){
#   for(i in seq_along(x$labels)){           # x (gg object) stores names of used columns as $labels
#     lab <- x$labels[[i]]                   # handle one label
#     if(length(lab)){                       # not null or empty expression
#       if(lab %in% names(x$data)){          # if this is just a bare column name
#         col <- x$data[[lab]]
#         atr <- attributes(col)
#         label <- atr$label                 # retrieve label
#         if(!is.null(label)){
#           x$labels[[i]] <- label           # replace default label with one from data attributes
#         }
#       }
#     }
#   }
#   # for(col in names(x$data)){
#   #   if(inherits(x$data[[col]],'dvec')){
#   #     x$data[[col]] <- unclass(x$data[[col]]) # class dvec confuses ggplot: "Don't know how to automatically pick scale for object of type dvec. Defaulting to continuous."
#   #   }
#   # }
#   NextMethod()
# }

# Enable Automatic Labels and Units for ggplot
#
# Enable automatic labels and units for ggplot.
# Substitutes column label, if present, for default.
# Supports arrangements of ggplot objects.
#
# @param x class 'ggready_ggplot' from \code{\link{ggplot.ggready}}
# @param ... passed arguments
# @return see \code{\link[ggplot2]{ggplot_build}}
# @export
# @importFrom ggplot2 ggplot_build
# @method ggplot_build ggready_ggplot
# @keywords internal
# @family ggready

# ggplot_build.ggready_ggplot <- print.ggready_ggplot


