#' Create tibble of all potential pressure combinations to test for interactions.
#'
#' \code{select_interaction} is a helper function that creates a tibble with
#' all indicator-specific combinations of pressures pairs as input for the
#' \code{\link{test_interaction}} function. The pressures in the IND~pressure
#' GAM(M)s are combined with all other pressures in the model tibble. If
#' specific combinations should not be modeled simply delete them from
#' this data frame.
#'
#' For each IND~pressure pair specific pressures to test for interactions can
#' be selected by creating a tibble containing the IND (termed `ind`), the
#' pressure 1 (termed `press``) and the pressure 2 (termed `t_var``). The easiest
#' is to use the helper function \code{\link{select_interaction}}: it creates
#' all combinations of IND~press pairs and the threshold variables based on
#' the input model tibble. If specific combinations should not be modeled
#' simply delete them from this data frame.
#'
#' @inheritParams test_interaction
#'
#' @return
#' The functions returns a tibble with three columns:
#' \describe{
#'   \item{\code{ind}}{The name of the indicator}
#'   \item{\code{press}}{The pressure name for the smooth term}
#'   \item{\code{t_var}}{The pressure name for the threshold variable}
#' }
#'
#' @export
#'
#' @examples
#' # Using some models of the Baltic Sea demo data
#' test <- select_interaction(mod_tbl = merge_models_ex[1:5,])

select_interaction <- function(mod_tbl) {

	 # Data input validation --------
	 if (missing(mod_tbl)) {
	 	 stop("Argument mod_tbl is missing.")
	 }

	 mod_tbl <- check_input_tbl(
				mod_tbl, tbl_name = "mod_tbl",
	 		parent_func = "model_gam() or model_gam()/select_gam() or calc_deriv()",
				var_to_check = c("id", "ind", "press"),
				dt_to_check = c("integer", "character", "character")
		)

	 # Check whether there are more than 1 pressure to test for interactions
	 if (length(unique(mod_tbl$press)) == 1) {
	 	 stop("mod_tbl contains only 1 pressure!")
	 }

	 # --------------

  # Create each possible combination for all
  # pressures
  pressures <- unique(mod_tbl$press) %>% merge(., .)
  names(pressures) <- c("t_var", "press")
  # Remove combinations of where t_var = pressures
  pressures <- pressures[pressures$t_var != pressures$press, ]
  # Convert factors into character vectors
  for (i in 1:2) {
    if (is.factor(pressures[, i])) {
      pressures[, i] <- as.character(pressures[, i])
    }
  }
  pressures <- suppressMessages(dplyr::left_join(mod_tbl[, c("ind",
    "press")], pressures))
  # Sort columns
  pressures <- dplyr::select_(pressures, "ind", "press",
    "t_var")


  return(pressures)
}
