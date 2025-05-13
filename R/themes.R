#' Set Plot Theme for Various Plot Objects
#'
#' Generic function to set a plot theme for different types of plot objects.
#'
#' @param x A plot object (e.g., a ggplot object).
#' @param template Character. The name of the theme template to apply (default:
#' "inrae").
#' @param ... Additional arguments passed to methods.
#'
#' @return A plot object with the specified theme applied.
#'
#' @export
#' @rdname themes-setPlotTheme
#'
setPlotTheme <- function(x, ...) {
  UseMethod("setPlotTheme", x)
}

#' @export
#' @rdname themes-setPlotTheme
#'
setPlotTheme.ggplot <- function(x, template = "inrae", ...) {
  x +
    getGgplotTheme(template = template, ...)
}

#' Get ggplot Theme
#'
#' Retrieves a ggplot2 theme based on the specified template.
#'
#' @inheritParams setPlotTheme
#' 
#' @return A ggplot2 theme object.
#'
#' @export
#' @rdname themes-getPlotTheme
#'
getGgplotTheme <- function(template = "inrae", ...) {
  if (!template %in% c("inrae")) createGgplotTheme(template = template, ...)
  switch(
    template,
    "inrae" = {
      moreArgs <- list(...)
      base_size <- ifelse(
        "base_size" %in% names(moreArgs), moreArgs$base_size, 12
      )
      moreArgs$base_size <- NULL
      theme <- InraeThemes::theme_inrae(base_size = base_size) +
        do.call(theme, moreArgs) +
        theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
    }
  )

  return(theme)
}

#' Create a Custom ggplot Theme
#'
#' Creates a new ggplot2 theme template. Not implemented yet.
#'
#' @inheritParams setPlotTheme
#' @param save Logical. Whether to save the new theme (default: FALSE).
#'
#' @return A ggplot2 theme object.
#'
#' @export
#' @rdname themes-createTheme
createGgplotTheme <- function(template, ..., save = FALSE) {
  stop("Not implemented yet")
}
