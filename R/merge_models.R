#' Merging two model output tibbles.
#'
#' The function appends the second model output tibble to the first
#' while keeping all variables from both tibbles.
#'
#' @param mod_tbl1 Model output tibble created e.g. with \code{\link{model_gam}}.
#' @param mod_tbl2 Model output tibble created e.g. with \code{\link{select_model}}.
#'
#' @details
#' \code{merge_models} function applies internally the dplyr::bind_rows function so that columns
#' are matched by name, and any missing columns will be filled with NA. The function
#' has also some data validation incorporated to check for double entries.
#'
#' @return
#' \code{merge_models} returns the same type as the input including all columns of
#' both tibbles.
#'
#' @export
#'
#' @examples
#' # Using some models of the Baltic Sea demo data:
#' # Merging GAM and GAMM tibbles
#' test_ids <- 47:50 # choose subset
#' gam_tbl <- model_gam_ex[test_ids,]
#' gamm_tbl <- model_gamm(ind_init_ex[test_ids,], filter= gam_tbl$tac)
#' best_gamm <- select_model(gam_tbl, gamm_tbl)
#' merge_models(gam_tbl[gam_tbl$tac == FALSE,], best_gamm)
#'
#' # Merge 2 IND-specific GAM tibbles (where)
#' dat_init <- ind_init(
#'   ind_tbl = ind_ex[, c("TZA", "Cod")],
#'   press_tbl = press_ex[, c("Tsum", "Swin")],
#'   time = ind_ex[,1])
#' gam_tbl1 <- model_gam(dat_init[1:2, ])
#' # treat a subset differently, e.g. when setting k
#' gam_tbl2 <- model_gam(dat_init[3:4, ], k = 3)
#' merge_models(gam_tbl1, gam_tbl2)
merge_models <- function(mod_tbl1, mod_tbl2) {

	 # Data input validation --------------------------
	 if (missing(mod_tbl1)) {
	 	stop("Argument 'mod_tbl1' is missing.")
	 }
		if (missing(mod_tbl2)) {
	 	stop("Argument 'mod_tbl2' is missing.")
	 }
		# Check input tibbles
		mod_tbl1 <- check_input_tbl(mod_tbl1, tbl_name = "mod_tbl1")
		mod_tbl2 <- check_input_tbl(mod_tbl2, tbl_name = "mod_tbl2")

  # Check if certain ids are in both datasets and if yes if
	 # they also contain the same ind/press names
  ids <- c(unique(mod_tbl1$id), unique(mod_tbl2$id))
  n <- which(table(ids) > 1)
  if (length(n) > 0) {
    stop(paste0("The following ids (i.e. same ind-press combinations) occur in both tibbles more than once: ",
      paste0(n, collapse = ", ")))
  }

  # Check if certain ids are in mod_tbl1 more than
  # once
  ids <- mod_tbl1$id
  n <- which(table(ids) > 1)
  if (length(n) > 0) {
    stop(paste0("The following ids (i.e. ind-press models) occur in mod_tbl1 more than once: ",
      paste0(n, collapse = ", ")))
  }

  # Check if certain ids are in mod_tbl2 more than
  # once
  ids <- mod_tbl2$id
  n <- which(table(ids) > 1)
  if (length(n) > 0) {
    stop(paste0("The following ids (i.e. ind-press models) occur in mod_tbl2 more than once: ",
      paste0(n, collapse = ", ")))
  }

  # Check if certain ids are in each dataset more
  # than once
  ids <- c(mod_tbl1$id, mod_tbl2$id)
  n <- which(table(ids) > 1)
  if (length(n) > 0) {
    stop(paste0("The following ids (i.e. ind-press models) occur in at least one of the tibbles more than once: ",
      paste0(n, collapse = ", ")))
  }
  # --------------------------------------------

  # Check if the columns are the same in both
  # datasets before merging
  mod_tbl1_col <- names(mod_tbl1)
  mod_tbl2_col <- names(mod_tbl2)
  if (all(mod_tbl1_col %in% mod_tbl2_col) & all(mod_tbl2_col %in%
    mod_tbl1_col)) {
    dat <- dplyr::bind_rows(mod_tbl1, mod_tbl2)
  } else {
    dat <- dplyr::bind_rows(mod_tbl1, mod_tbl2)
    # any missing columns will be filled with NA
    message("Some columns are only present in one table and are filled with NAs in the other table.")
  }
  # Sort by id
  dat <- dplyr::arrange_(dat, "id")

  ### END OF FUNCTION
  return(dat)
}
