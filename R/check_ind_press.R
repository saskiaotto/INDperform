#' Check class and variable types of input table
#'
#' \code{check_ind_press} is a helper function for \code{\link{model_trend}}
#' and \code{\link{ind_init}}. It coerces the data into a data frame and
#' checks the type of each variable. If any of the variables is not a
#' number (double or integer) it will return an error message.
#'
#' @param x A data object containing only the indicator or pressure
#' variables (vector, matrix, dataframe or tibble).
#' @param input Indication whether x represents the indicator (default) or pressure variable(s).
#'  If x is a vector, 'input' determines the variable name when coerced to a data frame.
#'
#' @seealso \code{\link{model_trend}} and the \code{\link{ind_init}}
#'
#' @keywords internal
#' @export
check_ind_press <- function(x, input = "ind") {
		# Coerce vector or table class to dataframe
		if (is.vector(x)) {
				x <- data.frame(x)
				if (input == "ind") {
					 names(x) <- "ind"
				} else {
					 names(x) <- "press"
				}
		} else {
				if( is.matrix(x) || tibble::is.tibble(x) ) x <- as.data.frame(x)
		}

		# Check type of each variable
		check_num <- purrr::map_lgl(x, is.numeric)
		if (!all(check_num)) {
			stop("All indicator or pressure variables have to be NUMERIC!")
		}
		return(x)
}
