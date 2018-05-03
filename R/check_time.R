#' Check class and data type of time vector
#'
#' \code{check_time} is a helper function for \code{\link{model_trend}},
#' \code{\link{ind_init}} and \code{\link{statespace_ed}}. It checks whether the
#' time object is a numeric vector. If not (a vector OR numeric) it will return an
#' error message.
#'
#' @param x An R object, which should be a numeric vector.
#'
#' @seealso \code{\link{model_trend}} and the \code{\link{ind_init}}
#'
#' @keywords internal
#' @export
check_time <- function(x) {
		# Check whether x is a factor
		if (is.factor(x)) {
			stop("'time' has to be a numeric vector, not a factor!")
		} else {
				# Check whether x is a vector
				if (is.vector(x) == FALSE) {
					stop("'time' has to be a VECTOR!")
				} else {
						# Check whether x is numeric
						if (is.numeric(x) == FALSE) {
							stop("'time' has to be a NUMERIC vector!")
						}
				}
		}
	 return(x)
}


