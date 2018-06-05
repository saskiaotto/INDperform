#' Check class and data type of input vector
#'
#' \code{check_input_vec} is a helper function for \code{\link{model_trend}},
#' \code{\link{ind_init}}, \code{\link{statespace_ed}} and \code{\link{statespace_ch}}.
#'  It checks whether the time object is an integer vector. If not (a vector
#' OR integer) it will return an error message.
#'
#' @param x An R object, which should be a numeric vector.
#' @param vec_name The name of the input object for the error message.
#'
#' @return
#' The function returns the checked input vector unchanged.
#'
#' @seealso \code{\link{model_trend}}, \code{\link{ind_init}},
#'  \code{\link{statespace_ed}} and \code{\link{statespace_ch}}
#'
#' @keywords internal
#' @export
check_input_vec <- function(x, vec_name) {

  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
    abs(x - round(x)) < tol
  }

  # Check whether x is a factor
  if (is.factor(x)) {
    stop(paste0(vec_name, " has to be an integer vector, not a factor!"))
  }
  # Check whether x is a vector
  if (is.vector(x) == FALSE) {
    stop(paste0(vec_name, " has to be a VECTOR!"))
  }
  # Check whether x is numeric
  if (is.numeric(x) == FALSE) {
    stop(paste0(vec_name, " has to be an INTEGER vector!"))
  }
  # If x represents a time vector check if integer
  if (vec_name == "time") {
    if (!all(is.wholenumber(x))) {
      stop(paste0("time has to be an INTEGER vector!"))
    } else {
      # Check if any time step is missing in vector
      time_seq <- x[-1] - x[-length(x)]
      if (length(unique(time_seq)) != 1) {
        message(paste0("The time vector is not evenly spaced. This will cause ",
          "erroneous results when testing and  modeling temporal autocorrelation! ",
          "To fix this a) add in your time vector the missing time step and b) in ",
          "ind_tbl or press_tbl corresponding rows with NAs. ",
          "Your time step intervals are: "))
        print(time_seq)
        stop()
      }
    }
  }
  return(x)
}
