#' Alternative method for confidence interval approximations of derivatives
#'
#' \code{approx_deriv} implements a crude approximation for the uncertainty
#' around the first derivatives. It should be used complementarily to the
#' conditional boostrap, if problems with GAMM fittings occur
#' (see \code{\link{calc_deriv}}).
#'
#' @inheritParams calc_deriv
#'
#' @details
#' In this approach derivatives are calculated
#' for the original smoother and some level of uncertainty (not exactly the
#' confidence intervals) is estimated based on the standard error (s.e.)
#' of the smoother. The same proportion of error (estimated as the ratio
#' s.e./fitted mean) is adopted for the maximal slope of the derivative and
#' then kept constant across the entire curve. As this results in much smaller
#' uncertainty ranges, a conversion (or multiplication) factor is implemented
#' to allow modifications of the error proportion. The default of 25 is a
#' compromise representing fairly well the results obtained for the GAMs
#' from the conditional bootstrap.
#'
#' @return
#' The function returns the input model tibble with the following 4 columns added
#' \describe{
#'   \item{\code{press_seq}}{A list-column with sequences of evenly spaced pressure
#'              values (with the length of the time series).}
#'   \item{\code{deriv1}}{A list-column with the first derivatives of the indicator
#'              responses averaged across all bootstraps.}
#'   \item{\code{deriv1_ci_up}}{A list-column with the upper confidence limit of the
#'              bootstrapped first derivatives.}
#'   \item{\code{deriv1_ci_low}}{A list-column with the lower confidence limit of the
#'              bootstrapped first derivatives.}
#' }
#'
#'
#' @seealso the wrapper function \code{\link{calc_deriv}}
#'
#' @export
#'
#'
#' @examples
#' # Using some models of the Baltic Sea demo data
#' init_tbl <- ind_init_ex[ind_init_ex$id %in% c(5,9,75), ]
#' mod_tbl <- merge_models_ex[merge_models_ex$id  %in% c(5,9,75), ]
#' deriv_tbl <- approx_deriv(init_tbl, mod_tbl, ci_prop_se = 25)
approx_deriv <- function(init_tbl, mod_tbl, ci_prop_se) {

  dat <- dplyr::left_join(mod_tbl, init_tbl, by = c("id",
    "ind", "press"))

  # Generate a sequence of pressure values for the
  # predictions
  dat$press_seq <- purrr::map(.x = dat$press_train,
    .f = ~seq(min(.x, na.rm = TRUE), max(.x, na.rm = TRUE),
      length.out = length(.x)))

  # Predicted values and ci proportion of original
  # GAM(M) (using the helper function calc_pred)
  # -------------

  pred_list <- calc_pred(dat$model, dat$press_seq)
  dat$pred <- pred_list$pred
  # Get se of fitted model
  se <- pred_list %>% purrr::map2(.x = .$ci_up, .y = .$pred,
    .f = ~(.x - .y)/1.96)
  mean_prop_se <- purrr::map2(.x = se, .y = dat$pred,
    .f = ~.x/.y) %>% purrr::map(mean)


  # Calculate 1st (F1) central derivatives
  # --------------

  dat$delta <- purrr::map_dbl(.x = dat$press_seq,
    .f = ~diff(seq(from = min(.x), to = max(.x),
      length.out = length(.x))[1:2]))
  dat$xp <- dat %>% purrr::map2(.x = .$press_seq,
    .y = .$delta, .f = ~.x + .y)
  dat$xm <- dat %>% purrr::map2(.x = .$press_seq,
    .y = .$delta, .f = ~.x - .y)
  dat$xp_pred <- calc_pred(dat$model, dat$xp)$pred
  dat$xm_pred <- calc_pred(dat$model, dat$xm)$pred

  dat$deriv1 <- purrr::pmap(.l = list(x = dat$xp_pred,
    y = dat$xm_pred, z = dat$delta), function(x,
    y, z) as.vector((x - y)/(2 * z)))
  # (pipe operator and formula do not work with pmap)


  # Calculate some approximation for CIs ------------

  # Use as CI a constant value that is added to every
  # deriv value. That constant is based on the mean
  # proportion of the s.e. to the fitted y and
  # multiplied by the conversion factor ci_prop_ci to
  # get a proportion value of the strongest slope.
  prop_ci_corr <- purrr::map_dbl(.x = mean_prop_se,
    .f = ~.x * 25)
  max_slope <- purrr::map_dbl(.x = dat$deriv1, .f = ~abs(max(.x)))
  ci_val <- purrr::map2_dbl(.x = max_slope, .y = prop_ci_corr,
    .f = ~.x * .y)

  dat$deriv1_ci_low <- purrr::map2(.x = dat$deriv1,
    .y = ci_val, .f = ~.x - .y)
  dat$deriv1_ci_up <- purrr::map2(.x = dat$deriv1,
    .y = ci_val, .f = ~.x + .y)

  # Remove various columns
  out <- dat[, c(1:17, 25, 32:34)]
  # 1:17 are original variables in mod_tbl, incl.
  # press_seq and deriv1/ci

  ### END OF FUNCTION
  return(out)
}
