#' Leave-One-Out Cross-Validation (LOOCV) procedure for (threshold-)GAMs
#'
#' \code{loocv_thresh_gam} applies a LOOCV on a threshold-GAM and its corresponding GAM and
#' returns TRUE if the threshold-GAM has a lower estimate, else FALSE (see for more infos on
#' the LOOCV procedure the details section in \code{\link{test_interaction}}).
#'
#' @inheritParams thresh_gam
#' @param time A vector containing the actual time series.
#'
#' @return
#' The function returns a list with the following 2 sublists:
#' \describe{
#'   \item{\code{result}}{logical; if TRUE, at least one thresh_gam
#'              performs better than its corresponding gam based on LOOCV value.}
#'   \item{\code{error}}{A string capturing potential error messages that
#'              occurred as side effects when fitting the threshold GAM for the
#'              LOOCV.}
#' }
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
#'   t_var_vec = ind_init_ex$press_train[[2]],
#'   name_t_var = "Swin",
#'   k = 4, a = 0.2, b = 0.8,
#'   time = ind_init_ex$time_train[[1]])
loocv_thresh_gam <- function(model, ind_vec, press_vec,
  t_var_vec, name_t_var, k, a, b, time) {

  # Data input preparation -------------------

  # Combine input vectors and create subsamples!
  data <- tibble::tibble(ind = ind_vec, press = press_vec,
    t_var = t_var_vec, time = time)

  # Create as many subtibbles as there are
  # observations with one observation left out in
  # each (used for the prediction and the MSE
  # calculation )
  data_train <- purrr::map(1:nrow(data), ~data[-.,
    ])
  data_test <- purrr::map(1:nrow(data), ~data[.,
    ])

  # Internal check:
  if (!all(purrr::map2_lgl(.x = data_train, .y = data_test,
    ~all(c(.x$time, .y$time) %in% time)))) {
    stop("Data splitting with exclusion of 1 obs did not work!")
  }

  # Create model lists and result tibble
  thresh_gam_list <- vector(mode = "list", length = length(time))
  dat <- tibble::tibble(gams_pred = rep(NA_real_,
    length(time)), thresh_gams_pred = rep(NA_real_,
    length(time)), observation = rep(NA_real_,
    length(time)))

  # Capture family and link from the input
  family <- mgcv::summary.gam(model)$family[[1]]
  if (stringr::str_detect(family, "Negative Binomial")) {
    family <- "nb"
  }
  link <- mgcv::summary.gam(model)$family[[2]]

  # To capture error messages of thresh_gam:
  thresh_gam_safe <- purrr::safely(thresh_gam, otherwise = NA)

  # Model fitting ------------------------------

  # Loop with progress bar
  pb <- dplyr::progress_estimated(length(time))
  # (initializes progress bar)
  for (i in seq_along(time)) {
    # Create input for model (formula cannot handle
    # [[]])
    ind <- data_train[[i]]$ind
    press <- data_train[[i]]$press
    t_var <- data_train[[i]]$t_var
    test <- data_test[[i]]

    # Normal GAMM
    dat_gam <- data.frame(ind = ind, press = press,
      t_var = t_var)
    formula_gam <- paste0("ind ~ 1 + s(press, k = ",
      k, ") + s(t_var, k = ", k, ")")
    # don't capture error for GAMs -> will occurr
    # anyway also in thresh_gam
    gam <- try(mgcv::gam(formula = stats::as.formula(formula_gam),
      data = dat, family = paste0(family, "(link = ",
        link, ")")), silent = TRUE)

    # threshold-GAMM - apply safe version for capturing
    # errors
    thresh_gam_list[[i]] <- thresh_gam_safe(ind_vec = ind,
      press_vec = press, t_var = t_var, name_t_var = name_t_var,
      k = k, a = a, b = b, model = model)

    # Capture output
    if (is.null(thresh_gam_list[[i]]$error)) {
      dat$gams_pred[[i]] <- mgcv::predict.gam(gam,
        newdata = test)
      # Add t_val to test_data for prediction
      test$modifier_level <- thresh_gam_list[[i]]$result$mr
      # Names has to be the same as in formula
      names(test)[c(1:3)] <- c(all.vars(model$formula),
        name_t_var)
      dat$thresh_gams_pred[[i]] <- mgcv::predict.gam(thresh_gam_list[[i]]$result,
        newdata = test)
      dat$observation[i] <- as.numeric(test[,
        1])
    }
    # Increment progress bar
    pb$tick()$print()
  }  # end of loop
  # Stop progress bar
  pb$stop()

  # Function to calculate the LOOCV by averaging
  # across squared errors of each test observation
  calc_loocv <- function(pred, obs) {
    diff <- pred - obs
    diff_sq <- diff^2
    mse <- mean(diff_sq, na.rm = TRUE)
    loocv_val <- signif(mse^0.5, 4)
    return(loocv_val)
  }

  # Calculate LOOCV only if number of all thresh_gams
  # were fitted successfully
  if (!any(is.na(thresh_gam_list[[i]]$result))) {
    if (calc_loocv(dat$gams_pred, dat$observation) >
      calc_loocv(dat$thresh_gams_pred, dat$observation)) {
      out_result <- TRUE
    } else {
      out_result <- FALSE
    }
  } else {
    out_result <- NA
  }

  # Save and return also all error messages
  out_error_list <- thresh_gam_list %>% purrr::transpose() %>%
    .$error
  if (all(purrr::map_lgl(out_error_list, .f = is.null))) {
    out_error <- NA
  } else {
    out_error <- purrr::map(out_error_list, ~.$message) %>%
      unlist() %>% unique() %>% paste(., collapse = "; ")
  }


  ### END OF FUNCTION
  out <- list(result = out_result, error = out_error)

  return(out)
}
