#' Generic plot functions
#'
#' This function is a generic method for plotting objects.
#'
#' @param x An object to be plotted.
#' @param ... Additional arguments to be passed to the plotting method.
#'
#' @return A plot of the object `x`.
#'
#' @export
#'
#' @examples
#' # Defaults
#' kplot(1:10)
#'
kplot <- function(x, ...) {
  UseMethod("kplot", x)
}

#' @export
#' @rdname kplot
#'
kplot.default <- function(x, ...) {
  plot(x, ...)
}

#' @export
#' @rdname kplot
#'
kplot.data.frame <- function(
  x, interactive = interactive(), output_format, ...
) {

}
