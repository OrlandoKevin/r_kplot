#' Plot data with ggplot
#'
#' @description
#' This function is a generic method for plotting objects using ggplot2.
#'
#' @param x An object to be plotted.
#' @param ... Additional arguments to be passed to the plotting method.
#'
#' @export
#'
#' @import ggplot2
#' @importFrom dplyr rename
#' @importFrom tidyr pivot_longer
#'
kggplot <- function(x, ...) {
  UseMethod("kggplot", x)
}

#' @export
#' @rdname kggplot
#'
kggplot.data.frame <- function(
  x, vars, labels, ...
) {
  if (vars$y == "all") vars$y <- colnames(x)[!colnames(x) == vars$x]
  if (length(vars$y) == 1) {
    x <- x %>%
      rename(Values = .data[[vars$y]])
  } else {
    x <- x %>%
      pivot_longer(any_of(vars$y), names_to = "vars", values_to = "values")
  }
}

#' @export
#' @rdname kggplot
#'
kggplot.kggplot <- function(x, ...) {

}
