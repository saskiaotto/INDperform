#' Calculates the Normalized Root Mean Square Error (NRMSE) as absolute value.
#'
#' \code{calc_nrmse} is a helper function for \code{\link{model_gam}} and
#' \code{\link{model_gamm}} and calculates the NRMSE from the test data.
#' The test MSE and test RMSE are commonly used tools for measuring the quality of
#' the model fit on previously unseen data. The normalization to the mean of
#' the observed test data allows for comparisons and a general scoring of the
#' model robustness across INDs with different scales or units. The absolute
#' values are needed for the \code{\link{scoring}} function.
#'
#' @param pred A list of vectors with predicted values.
#' @param obs_ind A list of vectors with observed values.
#'
#' @return
#' The function returns a numerical vector of the same length then the
#' input lists, with one NRMSE value for each GAM(M).
#'
#' @seealso \code{\link{model_gam}} and \code{\link{model_gamm}}
#'
#' @keywords internal
#' @export
#'
#' @examples
#' # Using the Baltic Sea demo data
#' data <- dplyr::left_join(model_gam_ex, ind_init_ex)
#' pred <- calc_pred(data$model, data$press_test)
#' x <- calc_nrmse(pred = pred$pred, obs_ind = data$ind_test)
calc_nrmse <- function(pred, obs_ind) {
  # Look for empty rows (missing models)
  choose <- purrr::map_chr(pred, class) == "array"
  nrmse <- vector(mode = "list", length = length(choose))

  # Squared Residuals
  for (i in 1:length(pred)) {
    if (choose[i]) {
      nrmse[[i]] <- (obs_ind[[i]] - pred[[i]])^2
    }
  }
  incl <- purrr::map_if(nrmse, choose, ~ !is.na(.) )
  obs_ind_incl <- purrr::map2(obs_ind, incl, ~ .x[.y] )
  # Column sums
  nrmse <- purrr::map_dbl(nrmse, ~sum(., na.rm = TRUE))
  # Divide by n (length(obs_ind_incl))
  nrmse <- purrr::map2(nrmse, obs_ind_incl, ~.x / length(.y) )
  # Get square root
  nrmse <- purrr::map_dbl(nrmse, ~sqrt(.))
  # Get mean for each vector in obs_ind_incl
  mean_obs_ind <- purrr::map_dbl(obs_ind_incl, .f = mean, na.rm = TRUE)
  # Divide by mean of respective column of
  # observations
  nrmse <- purrr::map2_dbl(.x = nrmse, .y = mean_obs_ind,
    .f = ~.x/.y)
  # Get absolute values (for scoring)
  nrmse <- abs(nrmse)
  # Replace missing models with NA
  nrmse[!choose] <- NA
  # Replace NaN with NA
  nrmse[is.nan(nrmse)] <- NA

  return(nrmse)
}
