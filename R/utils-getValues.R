#' Get Aesthetic Value from Data
#'
#' This function retrieves a specific aesthetic value from a data frame or list.
#' If the specified aesthetic is not found, it returns NULL.
#'
#' @param data A data frame or list containing the aesthetic values.
#' @param which A character string specifying the name of the aesthetic to retrieve.
#'
#' @return The value of the specified aesthetic from the data, or NULL if not found.
#'
#' @export
#' @rdname utils-getValues
#'
getValues <- function(data, which) {
  if (is.null(which)) {
    return(NULL)
  }
  return(data[[which]])
}
