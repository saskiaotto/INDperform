#' Extract specific outputs from the GAM(M) summary
#'
#' \code{get_sum_output} is a helper function for \code{\link{model_trend}},
#' \code{\link{model_gam}}, and \code{\link{model_gamm}} and extracts from a list
#' of \code{summary.gam} objects specific values defined in `varname`.
#'
#' @param sum_list A list of summary objects created with \link{summary.gam}.
#' @param varname A character naming the element to extract from the `sum_list`.
#' @param cell If more than one value is stored under `varname` you need to specify
#'  which one you want to pull with `cell`.
#'
#' @return
#' The function returns a vector with the length of `sum_list` containing the
#' extracted values.
#'
#' @seealso\code{\link{model_trend}}, \code{\link{model_gam}},
#'  \code{\link{model_gamm}}
#'
#' @keywords internal
#' @export
#'
#' @examples
#' # Using some models of the Baltic Sea demo data:
#' sum_list <- purrr::map(model_gam_ex$model, ~mgcv::summary.gam(.) )
#' get_sum_output(sum_list, varname = "edf")
#'
#' # Get p-val with cell argument:
#' get_sum_output(sum_list, "s.table", cell = 4)
get_sum_output <- function(sum_list, varname, cell = NULL) {

  # Data input validation ----------

  # Check if requested element contains only one value
  # and if cell is NULL
  ok <- which(!is.na(sum_list))
  if (is.null(cell) & length(sum_list[[ok[1]]][[varname]]) >
    1) {
    stop("The requested summary element contains more than one value. Select\n\t\t\tthe specific value using the cell argument")
  } else {
    if (is.null(cell))
      cell <- 1
  }
  # --------------------------------

  # Some models may not be fitted and are passed as
  # NA.
  choose <- !is.na(sum_list)
  # capture output
  result <- purrr::map_if(sum_list, choose, ~.[[varname]][cell])
  # check output
  if (any(purrr::map_dbl(result, length) != 1)) {
    stop("I don`t know what happened, but it`s not ok!")
  }
  return(unlist(result))
}
