#' Select the best correlation structure in the GAMM
#'
#' The function selects and returns the best GAMM out of the six GAMMs computed in
#' \code{\link{model_gamm}}. In the case that the GAMM without any correlation
#' structure performs best, the output tibble contains the information from
#' the original \code{\link{model_gam}} output tibble (therefore needed as input).
#'
#' @param gam_tbl Output tibble from the \code{\link{model_gam}} function.
#' @param gamm_tbl Output tibble from the \code{\link{model_gamm}} function.
#'
#' @details
#' The best error structure is chosen here based on the Akaike`s Information
#' Criterion (AIC). The GAMM with the lowest AIC value is selected, but only if the
#' AIC difference to the GAMMs with a less complex error structure is greater than 2
#' (or respectively 4 or 6 depending on the level of nested complexity)
#' (Burnham and Anderson, 2002). Otherwise the less complex GAMM is chosen.
#' The following hierarchy of complexity is considered:
#' \itemize{
#'   \item no structure < AR1 < AR2 and ARMA1,1 < ARMA2,1 and ARMA1,2
#'   }
#'
#' @return
#' \code{select_model} returns the same model output tibble as \code{\link{model_gamm}}
#' but with only \strong{one} final GAMM for each filtered IND~pressure pair.
#'
#' @references
#' Burnham, K.P., Anderson, D.R. (2002) Model Selection and Multimodel Inference - A
#' Practical Information-Theoretic Approach. Springer-Verlag New York.
#'
#' @family IND~pressure modeling functions
#'
#' @export
#'
#' @examples
#' # Using some models of the Baltic Sea demo data
#' test_ids <- c(63:70)
#' gam_tbl <- model_gam_ex[model_gam_ex$id %in% test_ids,]
#' gamm_tbl <- model_gamm(ind_init_ex[test_ids,], filter = gam_tbl$tac)
#' best_gamm <- select_model(gam_tbl, gamm_tbl)
select_model <- function(gam_tbl, gamm_tbl) {

  # Data input validation -----------------------
  if (missing(gam_tbl)) {
    stop("Argument gam_tbl is missing.")
  }
  if (missing(gamm_tbl)) {
    stop("Argument gamm_tbl is missing.")
  }
  # Check input tibbles
  gam_tbl <- check_input_tbl(gam_tbl, tbl_name = "gam_tbl",
    parent_func = "model_gam()", var_to_check = c("id",
      "ind", "press", "aic"), dt_to_check = c("integer",
      "character", "character", "numeric"))
  gamm_tbl <- check_input_tbl(gamm_tbl, tbl_name = "gamm_tbl",
    parent_func = "model_gamm()", var_to_check = c("id",
      "ind", "press", "aic"), dt_to_check = c("integer",
      "character", "character", "numeric"))

  # Test if all gamm_ids occur in the gam_tbl
  if (!all(gamm_tbl$id %in% gam_tbl$id)) {
    id <- which(!unique(gamm_tbl$id) %in% gam_tbl$id)
    stop(paste0("The following ids are missing in gam_tbl but exist in gamm_tbl: ",
      paste(unique(gamm_tbl$id)[id], collapse = ",")))
  }
  # ----------------

  final_tab <- tibble::tibble(id = unique(gamm_tbl$id))

  model_list <- split(gamm_tbl, gamm_tbl$id)

  # helper function
  get_best_model <- function(models) {
    min_aic <- models$aic + c(0, 2, 4, 4, 6, 6)
    pos_min <- which(min_aic == suppressWarnings(min(min_aic,
      na.rm = TRUE)))
    best_mod <- models$corrstruc[pos_min]
    if (best_mod == Inf)
      best_mod <- "none"
    return(best_mod)
  }

  # get best corrstruc
  corrstruc <- purrr::map_chr(model_list, get_best_model)
  # initialise corrstruc_col with 'none'
  final_tab$corrstruc <- "none"
  # fill column with corrstruc
  for (i in gamm_tbl$id) {
    final_tab$corrstruc[final_tab$id == i] <- corrstruc[which(names(corrstruc) ==
      as.character(i))]
  }

  # merge gam(m)_tbl to final_tab
  suppressMessages(final_tab1 <- dplyr::left_join(final_tab[final_tab$corrstruc ==
    "none", ], gam_tbl))
  suppressMessages(final_tab2 <- dplyr::left_join(final_tab[!final_tab$corrstruc ==
    "none", ], gamm_tbl))

  # combine final_tab1 and 2
  final_tab <- dplyr::bind_rows(final_tab1, final_tab2)
  # sort rows by ids
  final_tab <- dplyr::arrange_(final_tab, .dots = "id")
  # arrange cols
  final_tab <- sort_output_tbl(final_tab)

  ### END OF FUNCTION
  return(final_tab)
}
