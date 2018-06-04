#' Check class, existing variables and data types of input objects
#'
#' \code{check_input_tbl} is a helper function for various modeling and
#' scoring functions in `INDperform`. It checks whether the input object
#' is of class tibble (should be if an output of one of the required functions)
#' and if all required variables are included in the tibble with the correct
#' data type. If not, an error message will be returned.
#'
#' @param x An output tibble from one of the INDperform modeling or scoring
#'  functions.
#' @param tbl_name The name of `x` for the error message.
#' @param parent_func The name of the function that generates the output tibble `x`.
#' @param var_to_check A character vector listing the variables to check for in `x`.
#' @param dt_to_check A character vector listing the data types of each variable
#'  listed in var_to_check (has to be the same order!) to check for.
#'
#' @return
#' The function returns the checked input tibble unchanged.
#'
#' @keywords internal
#' @export
check_input_tbl <- function(x, tbl_name, parent_func = NULL, var_to_check = NULL,
	 dt_to_check = NULL) {

 	# Check if x is tibble
		if (!tibble::is.tibble(x)) {
			 if (!is.null(parent_func)) {
				  stop(paste0("The input object ", tbl_name, " has to be a tibble (output from ",
				    parent_func, ")!"))
			 } else {
				  stop(paste0("The input object ", tbl_name, " has to be a tibble!"))
			 }
		}

  # Check if tibble contains all needed variables and data types
		if (!is.null(var_to_check)) {
			 if (any(!var_to_check %in% names(x))) {
			 		missing_var <- var_to_check[!var_to_check %in% names(x)]
			 		stop(paste0("The following variables required for this function are missing in ",	tbl_name,
			 				" (maybe you forgot to run another function on this tibble or removed these variables): ",
			 				paste0(missing_var, collapse = ", ")))
			 } else {
			   if (!is.null(dt_to_check)) {
			 	   # Check if variables have the correct data type
				    dt <- rep(FALSE, length(var_to_check))
				    for (i in seq_along(var_to_check)) {
						    if (class(x[[var_to_check[i]]]) == dt_to_check[i] ) dt[i] <- TRUE
				    }
				    if (any(dt == FALSE)) {
									 wrong_dt <- dt_to_check[dt == FALSE]
									 var_wrong_dt <- var_to_check[dt == FALSE]
									 message(paste0("The following variables have not the required data types in ", tbl_name, ":"))
									 print(data.frame(variable = var_wrong_dt, required_data_type = wrong_dt))
									 stop()
								}
			 		}
			 }
		}

  return(x)
}
