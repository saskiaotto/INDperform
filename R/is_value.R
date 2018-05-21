#' Returns a logical vector whether input
#'
#' \code{is_value} is a helper function for \code{\link{model_gamm}},
#' \code{\link{calc_deriv}},
#' \code{\link{test_interaction}}, and \code{\link{scoring}}.
#' It checks whether each element is a true value and not NA, NaN, NULL or
#' Inf when filtering tibbles for specific values. If the filtering variable (e.g.
#' p_val or edf) contains NAs, the respective rows are usually not excluded in
#' the requested subset or (when using the function \code{split}) excluded in both
#' TRUE/FALSE subsets.
#'
#' @param x An R object, which should be a numeric vector.
#'
#' @return
#' The function returns a logical vector with TRUE or FALSE for each element of x.
#'
#' @keywords internal
#' @export
is_value<- function(x) {
	out <- !(is.na(x) | is.nan(x) |
              is.null(x) | is.infinite(x))
	return(out)
}



