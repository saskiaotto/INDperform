#' Check class and variable types of input table
#'
#' UPDATE DOCUMENTATION!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#'
#' \code{check_input_tbl} is a helper function for \code{\link{model_trend}}
#' and \code{\link{ind_init}}. It coerce the data table into a data frame
#' if needed and checks the type of each variable. If any of the variables is not
#' a number (double or integer) it will return an error message.
#'
#' @param x A table object (dataframe, tibble, or matrix).
#'
#' @seealso \code{\link{model_trend}} and the \code{\link{ind_init}}
#'
#' @keywords internal
#'  x = ind_init_ex
#'  tbl_name = "init_tbl"
#'  parent_func = "ind_init()"
#'  var_to_check = names(x)[1:4]
#'  dt_to_check = letters[1:4]# dt2c#unlist(purrr::map(1:3, ~ typeof(x[[.]])))
#' @export
check_input_tbl <- function(x, tbl_name, parent_func = NULL, var_to_check = NULL,
	 dt_to_check = NULL) {

 	# Check if x is tibble
		if (!tibble::is.tibble(x)) {
			 if (!is.null(parent_func)) {
				  stop(paste0("The input object '", tbl_name, "' has to be a tibble (output from ",
				    parent_func, ")!"))
			 } else {
				  stop(paste0("The input object '", tbl_name, "' has to be a tibble!"))
			 }
		}

  # Check if tibble contains all needed variables and data types
		if (!is.null(var_to_check)) {
			 if (any(!var_to_check %in% names(x))) {
			 		missing_var <- var_to_check[!var_to_check %in% names(x)]
			 		stop(paste0("The following variables required for this function are missing in '",	tbl_name,
			 				"' (maybe you forgot to run another function on this tibble or removed these variables): ",
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
									 message(paste0("The following variables have not the required data types in '", tbl_name, "':"))
									 print(data.frame(variable = var_wrong_dt, required_data_type = wrong_dt))
									 stop()
								}
			 		}
    }
		}

  return(x)
}
