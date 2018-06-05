#' Extracts the IND~pressure IDs in the tibble.
#'
#' \code{find_id} is a helper function for the user to extract the ID for a
#' specific indicator(IND), pressure or IND~pressure combination. The `id` in
#' the returned tibble can then be used for filtering tibbles when using the
#' other IND~pressure modeling functions.
#'
#' @param mod_tbl A tibble containing the columns \code{ind} and \code{press.}.
#' @param ind_name One or more character string naming the indicators of interest.
#' @param press_name One or more character string naming the pressures of interest.
#'
#' @return
#' The function returns a tibble including the id for the respective `ind`
#' and/or `press`.
#'
#' @family IND~pressure modeling functions
#'
#' @export
#'
#' @examples
#' # Using the Baltic Sea demo data:
#' # Look for specific INDs in combination with every pressure
#' ind_name <- c("TZA","MS")
#' find_id(model_gam_ex, ind_name)$id
#' # Look for specific IND~pressure combinations
#' press_name <- c("Tsum", "Swin")
#' find_id(model_gam_ex, ind_name, press_name)
#' # Look for specific pressures in combination with every IND
#' find_id(model_gam_ex, press_name = press_name)
find_id <- function(mod_tbl, ind_name = NULL, press_name = NULL) {

  # Data input validation -------
  if (missing(mod_tbl)) {
    stop("Argument mod_tbl is missing.")
  }
  # Check input
  if (is.null(press_name)) {
    # Check input
    if (is.null(ind_name)) {
      res <- mod_tbl
    } else {
      # Look for matches with ind_name
      res <- dplyr::filter_(mod_tbl, ~ind %in%
        ind_name)
    }
  } else {
    # Check for matches with press_name
    if (is.null(ind_name)) {
      res <- dplyr::filter_(mod_tbl, ~press %in%
        press_name)
    } else {
      # Check for matches with both ind_ and press_name
      res <- dplyr::filter_(mod_tbl, ~press %in%
        press_name) %>% dplyr::filter_(~ind %in%
        ind_name)
    }
  }
  return(res)
}
