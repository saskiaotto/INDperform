#' Calculates the Normalized Root Mean Square Error (NRMSE) as absolute value.
#'
#' \code{calc_nrmse} is a helper function for \code{\link{model_gam}} and
#' \code{\link{model_gamm}} and calculates the NRMSE from the test data.
#' The test MSE and test RMSE are commonly used tools for measuring the quality of
#' the model fit on previously unseen data. The normalisation to the mean of
#' the observed test data allows for comparisons and a general scoring of the
#' model robustness across INDs with different scales or units. The absolute
#' values are needed for the \code{\link{scoring}} function.
#'
#' @param pred A list of vectors with predicted values.
#' @param obs_ind A list of vectors with obeserved values.
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
  # Column sums
  nrmse <- purrr::map_dbl(nrmse, ~sum(., na.rm = TRUE))
  # Divide by n (length(obs_ind))
  nrmse <- purrr::map_if(nrmse, choose, ~./length(obs_ind[[1]]))
  # Get square root
  nrmse <- purrr::map_dbl(nrmse, ~sqrt(.))
  # Get mean for each vector in obs_ind
  mean_obs_ind <- purrr::map_if(obs_ind, choose,
    mean, na.rm = TRUE)
  # Replace missing models with NA
  mean_obs_ind[!choose] <- NA
  # Divide by mean of respective column of
  # observations
  nrmse <- purrr::map2_dbl(.x = nrmse, .y = mean_obs_ind,
    .f = ~.x/.y)
  # Get absolute values (for scoring)
  nrmse <- abs(nrmse)

  return(nrmse)
}
