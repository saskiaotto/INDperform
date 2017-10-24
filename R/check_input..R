#' Check class and variable types of input table
#'
#' \code{check_input} is a helper function for \code{\link{model_trend}}
#' and \code{\link{ind_init}}. It coerce the data table into a data frame
#' if needed and checks the type of each variable. If any of the variables is not
#' a number (double or integer) it will return an error message.
#'
#' @param x A table object (dataframe, tibble, or matrix).
#'
#' @seealso \code{\link{model_trend}} and the \code{\link{ind_init}}
#'
#' @keywords internal
#' @export
check_input <- function(x) {
  # Coerce table class to dataframe
  if (is.matrix(x)) x <- as.data.frame(x)
  if (tibble::is.tibble(x)) x <- as.data.frame(x)

  # Check type of each variable
  check_num <- purrr::map_lgl(x, is.numeric)
  check_int <- purrr::map_lgl(x, is.integer)
  check_both <- purrr::map2_lgl(check_num, check_int,
    ~any(.x, .y))
  if (!all(check_both)) {
    stop("All columns have to be of class numeric or integer!")
  }
  return(x)
}
