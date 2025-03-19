#' Plot data with ggplot
#'
#' @description
#' This function is a generic method for plotting objects using ggplot2.
#'
#' @param x An object to be plotted.
#' @param vars [list] with the variables to be plotted.
#' @param labels [list] with the labels for the variables.
#' @param ... Additional arguments to be passed to the plotting method.
#'
#' @export
#'
#' @import ggplot2
#' @importFrom dplyr rename any_of
#' @importFrom tidyr pivot_longer
#'
kggplot <- function(x, ...) {
  UseMethod("kggplot", x)
}

#' @export
#' @rdname kggplot
#'
kggplot.data.frame <- function(
  x, plot_type, vars, labels = NULL, ...
) {
  if (vars$y == "all") vars$y <- colnames(x)[!colnames(x) == vars$x]
  if (length(vars$y) == 1) {
    x <- x %>%
      rename(Values = .data[[vars$y]])
  } else {
    x <- x %>%
      pivot_longer(any_of(vars$y), names_to = "vars", values_to = "values")
  }
  if (plot_type != "map") {
    plot <- ggplot(
      x,
      aes(
        x = .data[[vars$x]], y = .data$Values,
        color = getValues(.data, vars$color), fill = getValues(.data, vars$fill),
        group = getValues(.data, vars$group)
      )
    )
  }


  switch(
    plot_type,
    "point" = {
      plot <- plot +
        geom_point(...)
    },
    "line" = {
      plot <- plot +
        geom_line(...)
    },
    "density" = {
      plot <- plot +
        geom_density() +
        expand_limits(y = 0)
    },
    "cumFreq" = {
      plot <- plot +
        geom_line(stat = "ecdf")
    },
    "bar" = {
      plot <- plot +
        geom_bar(stat = "identity", ...)
    },
    "bar_dodge" = {
      plot <- plot +
        geom_bar(stat = "identity", width = 0.5, position = position_dodge(0.8))
    },
    "map" = {
      basemap <- createBasemap(model = "ggplot", ..., cfg = cfg)

      data <- merge(x, basemap$data, all.x = TRUE)

      plot <- basemap +
        geom_sf(data = data, aes(fill = .data[[aes$x]], color = .data[[aes$y]]))
    }
  )

  if (!is.null(labels)) {
    plot <- plot +
      do.call(labs, labels)
  }

  plot <- plot +
    theme_minimal()

  return(plot)
}

#' @export
#' @rdname kggplot
#'
kggplot.kggplot <- function(x, ...) {

}
