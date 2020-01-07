#' Check class and variable names and types of ind and press data
#'
#' \code{check_ind_press} is a helper function for \code{\link{model_trend}}
#' and \code{\link{ind_init}}. It coerces the data into a data frame and
#' checks the type of each variable. If any of the variables is not a
#' number (double or integer) it will return an error message. Also the
#' variable names are checked for unwanted characters (e.g. hyphen, brackets, etc.)
#' and when necessary replaced by an underscore. The latter is required for
#' the correct model specifications.
#'
#' @param x A data object containing only the indicator or pressure
#' variables (vector, matrix, data frame or tibble).
#' @param input Indication whether x represents the indicator (default) or
#'  pressure variable(s). If x is a vector, `input` determines the variable
#'  name when coerced to a data frame.
#'
#' @return
#' The function returns the input object as data frame with correct variable names.
#'
#' @seealso \code{\link{model_trend}} and the \code{\link{ind_init}}
#'
#' @keywords internal
#' @export
check_ind_press <- function(x, input = "ind") {
  # Coerce vector or table class to data frame
  if (is.vector(x)) {
    x <- data.frame(x)
    if (input == "ind") {
      names(x) <- "ind"
    } else {
      names(x) <- "press"
    }
  } else {
    if (is.matrix(x) || tibble::is_tibble(x))
      x <- as.data.frame(x)
  }

  # Check type of each variable
  check_num <- purrr::map_lgl(x, is.numeric)
  if (!all(check_num)) {
    stop("All indicator or pressure variables have to be NUMERIC!")
  }

  # Check for unwanted characters in variable names and replace
  # with hyphen if necessary
  check_var_names <- function(x) {
    # add an 'x' before starting numbers
    sel <- stringr::str_detect(x, "^\\d")
    x[sel] <- stringr::str_c("x", x[sel])
    # remove starting and ending punctuations and blanks
    x <- stringr::str_replace_all(x, "^[:punct:]+|[:punct:]+$|^[:blank:]+|[:blank:]+$", "")
    # replace punctuations (except for hyphens and points)
     x <- stringr::str_replace_all(x, "(\\!|\\?|\\(|\\)|\\{|\\}|\\[|\\]|[:blank:]|\\-|\\/|\\#|\\+|\\*|\\'|\\$|\\%|\\&|\\=|\\,|\\;|\\:)+",
       "_")
     return(x)
  }
  names(x) <- check_var_names(names(x))

  return(x)
}
