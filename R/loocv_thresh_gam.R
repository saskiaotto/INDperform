#' Leave-One-Out Cross-Validation (LOOCV) procedure for (threshold-)GAMs
#'
#' \code{loocv_thresh_gam} applies a LOOCV on a threshold-GAM and its corresponding GAM and
#' returns TRUE if the threshold-GAM has a lower estimate, else FALSE (see for more infos on
#' the LOOCV procedure the details section in \code{\link{test_interaction}}).
#'
#' @inheritParams thresh_gam
#' @param time A vector containing the actual time series.
#'
#' @seealso \code{\link{thresh_gam}} which creates a threshold-GAM object and
#' \code{\link{test_interaction}} which applies thresh_gam and loocv_thresh_gam
#'
#' @keywords internal
#' @export
#'
#' @examples
#' # Using the first model in the Baltic Sea demo data
#' loocv_thresh_gam(model = model_gam_ex$model[[1]],
#'   ind_vec = ind_init_ex$ind_train[[1]],
#'   press_vec = ind_init_ex$press_train[[1]],
#'   t_var = ind_init_ex$press_train[[2]],
#'   name_t_var = "Swin",
#'   k = 4, a = 0.2, b = 0.8,
#'   time = ind_init_ex$time_train[[1]])
loocv_thresh_gam <- function(model, ind_vec, press_vec,
  t_var, name_t_var, k, a, b, time) {

	 # Data input preparation -------------------

  # Combine input vectors and create subsamples!
  data <- tibble::tibble(ind = ind_vec, press = press_vec,
    t_var = t_var, time = time)

  # Create as many sub-tibbles as there are observations
  # with one observation left out in each (used for
  # the prediction and the MSE calculation	)
  data_train <- purrr::map(1:nrow(data),
  	 ~data[-., ])
  data_test <- purrr::map(1:nrow(data),
  	 ~data[., ])

  # Internal check:
  if (!all(purrr::map2_lgl(.x = data_train, .y = data_test,
    ~all(c(.x$time, .y$time) %in% time)))) {
    stop("Data splitting with exlusion of 1 obs did not work!")
  }

  # Create model lists and result tibble
  gams_pred <- thresh_gams_pred <- vector(mode = "list",
    length = length(time))
  dat <- tibble::tibble(
  	 gams_pred = vector(mode = "numeric",
    length = length(time)),
  	 thresh_gams_pred = vector(mode = "numeric",
    length = length(time)),
  	 observation = vector(mode = "numeric",
    length = length(time))
  	)

  # Capture family and link from the input
  family <- mgcv::summary.gam(model)$family[[1]]
  link <- mgcv::summary.gam(model)$family[[2]]

  # Model fitting ------------------------------

  # Loop with progress bar
  pb <- dplyr::progress_estimated(length(gams_pred))
		  # (initializes progress bar)
  for (i in seq_along(gams_pred)) {
    # Create input for model (formula cannot
    # handle [[]])
    ind <- data_train[[i]]$ind
    press <- data_train[[i]]$press
    t_var <- data_train[[i]]$t_var
    test <- data_test[[i]]

    # Normal GAMM
    dat_gam <- data.frame(ind = ind, press = press,
      t_var = t_var)
    formula_gam <- paste0("ind ~ 1 + s(press, k = ",
      k, ") + s(t_var, k = ", k, ")")
    family_gam <- mgcv::summary.gam(model)$family[[1]]
    link_gam <- mgcv::summary.gam(model)$family[[2]]
    gam <- mgcv::gam(formula = stats::as.formula(formula_gam),
      data = dat, family = paste0(family, "(link = ",
        link, ")"))

    # threshold-GAMM
    thresh_gam <- thresh_gam(ind_vec = ind, press_vec = press,
      t_var = t_var, name_t_var = name_t_var,
      k = k, a = a, b = b, model = model)

    # Capture output
    dat$gams_pred[[i]] <- mgcv::predict.gam(gam,
      newdata = test)
    # Add t_val to test_data for prediction
    test$modifier_level <- thresh_gam$mr
    # Names has to be the same as in formula
    names(test)[c(1:3)] <- c(all.vars(model$formula),
      name_t_var)
    dat$thresh_gams_pred[[i]] <- mgcv::predict.gam(thresh_gam,
      newdata = test)
    dat$observation[i] <- as.numeric(test[, 1])
    # Increment progress bar
    pb$tick()$print()
  } # end of loop
  # Stop progress bar
  pb$stop()

  # Calculate the LOOCV by averaging all (mean)
  # sqare errors of each test observation
  calc_loocv <- function(pred, obs) {
    diff <- pred - obs
    diff_sq <- diff^2
    mse <- mean(diff_sq, na.rm = TRUE)
    loocv_val <- signif(mse^0.5, 4)
    return(loocv_val)
  }

  if (calc_loocv(dat$gams_pred, dat$observation) >
    calc_loocv(dat$thresh_gams_pred, dat$observation)) {
    out <- TRUE
  } else {
    out <- FALSE
  }

  return(out)
}
