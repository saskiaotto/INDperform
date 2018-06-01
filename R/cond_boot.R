#' Conditional bootstraps
#'
#' \code{cond_boot} creates n_boot predicted IND time series based on a
#' conditional bootstrap for calculating the derivatives of the resulting
#' smoothing curves.
#'
#' @param ci Confidence interval of the boostrapped smoothing functions and their
#'   derivatives. Must be between 0 and 1, default is 0.95.
#' @inheritParams calc_deriv
#'
#' @details
#'
#' \code{cond_boot} produces first n_boot new IND time series by
#' resampling from the residuals of the original IND-Pressure GAM(M) and
#' adding these to the original IND time series repeatedly. For GAMMs the
#' correlation structure in the bootstrapped residuals is kept contant
#' by using the \code{\link{arima.sim}} function with the bootstrapped
#' residuals as times series of innovations and the correlation parameters
#' from the orginal model.
#' A separate GAM(M) is then fitted to each bootstrapped IND time series. If
#' errors occurr during the n_boot iterations of resampling and model fitting
#' (e.g., convergence errors for GAMMs), the process is repeated until n_boot
#' models have been fitted successfully.
#'
#'
#' The function calculates then the first derivatives of each bootstrapped
#' IND time series prediction and computes a mean and confidence intervals (CI)
#' of both IND predictions and derivatives. The CIs are computed by sorting the
#' n_boot bootstrapped derivatives into ascending order and calculating the
#' upper and lower percentiles defined by the \code{ci} argument (the default
#' is the 2.5\% and 97.5\% percentiles representing the 95\% CI).
#'
#'
#' The parallel computation in this function builds on the packages
#' \code{parallel} and \code{pbapply} with its function
#' \code{\link[pbapply]{pblapply}}. This allows the vectorized computations
#'  similar to lapply and adds further a progress bar.
#'
#' @return
#' The function returns the input model tibble with the following 9 columns added
#' \describe{
#'   \item{\code{press_seq}}{A list-column with sequences of evenly spaced pressure
#'              values (with the length of the time series).}
#'   \item{\code{pred}}{A list-column with the predicted indicator responses
#'              averaged across all bootstraps.}
#'   \item{\code{pred_ci_up}}{A list-column with the upper confidence limit of the
#'              bootstrapped predictions.}
#'   \item{\code{pred_ci_low}}{A list-column with the lower confidence limit of the
#'              bootstrapped predictions.}
#'   \item{\code{deriv1}}{A list-column with the first derivatives of the indicator
#'              responses averaged across all bootstraps.}
#'   \item{\code{deriv1_ci_up}}{A list-column with the upper confidence limit of the
#'              bootstrapped first derivatives.}
#'   \item{\code{deriv1_ci_low}}{A list-column with the lower confidence limit of the
#'              bootstrapped first derivatives.}
#'   \item{\code{adj_n_boot}}{The number of successful bootstrap samples that was
#'              actually used for calculating the mean and confidence intervals of
#'              the predicted indicator reponse and the derivative.}
#'   \item{\code{boot_error}}{A list-column capturing potential error messages that
#'              occurred as side effects when refitting the GAM(M)s on each bootstrap
#'              sample.}
#'  }
#'
#' @seealso the wrapper function \code{\link{calc_deriv}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  # Using some models of the Baltic Sea demo data
#'  init_tbl <- ind_init_ex[ind_init_ex$id %in% c(5,9,75), ]
#'  mod_tbl <- merge_models_ex[merge_models_ex$id  %in% c(5,9,75), ]
#'  deriv_tbl <- cond_boot(mod_tbl = mod_tbl, init_tbl = init_tbl,
#'				excl_outlier = TRUE, n_boot = 200,	ci = 0.95,
#'				par_comp = TRUE, no_clust = NULL, seed = NULL)
#'	}
cond_boot <- function(init_tbl, mod_tbl, excl_outlier,
  n_boot, ci, par_comp, no_clust, seed) {

  # Check if n_boot needs to be adjusted
  n_boot <- check_n_boot(n_boot = n_boot, ci = ci)

  # Data preparation -----------------------

  # Exclude outliers given in excl_outliers
  if (excl_outlier) {
    init_tbl$ind_train <- purrr::map2(init_tbl$ind_train,
      mod_tbl$excl_outlier, ~replace(.x, .y,
        NA))
    init_tbl$press_train <- purrr::map2(init_tbl$press_train,
      mod_tbl$excl_outlier, ~replace(.x, .y,
        NA))
    init_tbl$time_train <- purrr::map2(init_tbl$time_train,
      mod_tbl$excl_outlier, ~replace(.x, .y,
        NA))
  }

  dat <- dplyr::left_join(mod_tbl, init_tbl, by = c("id",
    "ind", "press"))

  # Generate a sequence of pressure values for the
  # predictions
  dat$press_seq <- purrr::map(.x = dat$press_train,
    .f = ~seq(min(.x, na.rm = TRUE), max(.x, na.rm = TRUE),
      length.out = length(.x)))
  # Get delta and generate press_seq plus/minus delta
  # for deriv calculation
  dat$delta <- purrr::map_dbl(.x = dat$press_seq,
    .f = ~diff(seq(from = min(.x), to = max(.x),
      length.out = length(.x))[1:2]))
  dat$xp <- dat %>% purrr::map2(.x = .$press_seq,
    .y = .$delta, .f = ~.x + .y)
  dat$xm <- dat %>% purrr::map2(.x = .$press_seq,
    .y = .$delta, .f = ~.x - .y)

  # Save edf as whole numbers for setting (fixed) k
  dat$dfF <- purrr::map(dat$edf, ceiling)

  # Add starter value depending on the corrstruc
  value_tib <- tibble::tibble(corrstruc = c("ar1",
    "ar2", "arma11", "arma12", "arma21"), values = list(c(0.3),
    c(0.3, -0.3), c(0.3, 0.3), c(0.3, -0.3, 0.3),
    c(0.3, 0.3, -0.3)))
  dat <- dplyr::left_join(dat, value_tib, by = c("corrstruc"))

  # Get model residuals (GAMs need to be refitted
  # with fixed k! With GAMMs it does not work and
  # the original model is used)
  calc_resid <- function(x, y, model, dfF) {
  	dat <- data.frame(x = x, y = y)
    if (class(model)[1] == "gam") {
      k <- dfF + 1
      family <- mgcv::summary.gam(model)$family[[1]]
      if (stringr::str_detect(family, "Negative Binomial")) {
      	 family <- "nb"
      }
      link <- mgcv::summary.gam(model)$family[[2]]
      fit <- mgcv::gam(stats::as.formula(paste0("y ~ s(x, k = ",
        k, ", fx = TRUE)")), family = paste0(family,
        "(link = ", link, ")"), data = dat)
      resid <- stats::residuals(fit)
    } else {
      if (is.list(model)) {
        # i.e. if gamm
        resid <- stats::residuals(model$lme,
          type = "normalized")
      } else {
        resid <- NA  # might not be needed if no fitting error occurrs
      }
    }
    return(resid)
  }
  calc_resid_safe <- purrr::possibly(calc_resid, otherwise = NA)

  dat$resid <- purrr::pmap(.l = list(x = dat$press_train,
    y = dat$ind_train, model = dat$model, dfF = dat$dfF),
    .f = calc_resid_safe)

  # Create list of correlation parameter for
  # arima.sim function (when creating bootstrapped
  # y's with GAMMs)
  cor_params <- list(ar1 = c("Phi", "Phi1"), ar2 = c("Phi1",
    "Phi2"), arma11 = c("Phi1", "Theta1"), arma12 = c("Phi1",
    "Theta1", "Theta2"), arma21 = c("Phi1", "Phi2",
    "Theta1"))
  get_arma_list <- function(model, cor_params) {
    if (class(model)[1] == "gam") {
      arma_list <- NA
    } else {
      cor_coef <- stats::coef(model$lme$modelStruct$corStruct,
        unconstrained = F)
      cor_param <- names(cor_coef)
      if (any(
      	identical(cor_param, cor_params[[1]][1]) | identical(cor_param, cor_params[[1]][2]) )) {
       # corARMA(1,0) writes it 'Phi1' (our setting), corAR1 writes it just 'Phi'
      	# so in casew user applies manual GAM both versions included
      	arma_list <- list(ar = cor_coef)
      }
      if (identical(cor_param, cor_params[[2]])) {
        arma_list <- list(ar = cor_coef)
      }
      if (identical(cor_param, cor_params[[3]])) {
        arma_list <- list(ar = cor_coef[1],
          ma = cor_coef[2])
      }
      if (identical(cor_param, cor_params[[4]])) {
        arma_list <- list(ar = cor_coef[1],
          ma = cor_coef[2:3])
      }
      if (identical(cor_param, cor_params[[5]])) {
        arma_list <- list(ar = cor_coef[1:2],
          ma = cor_coef[3])
      }
    }
    return(arma_list)
  }
  get_arma_list_safe <- purrr::possibly(get_arma_list, otherwise = NA)

  dat$arma_list <- purrr::map(.x = dat$model,
    .f = get_arma_list_safe, cor_params = cor_params)


  # Actual conditional bootstrap per id -----------------

  # Helper functions that generate y_boot,
  # refits models, calculates preds and derivs,
  # averages across bootstraps, including ci's

  boot_y <- function(y, resid, model, arma_list) {
    if (class(model)[1] == "gam") {
      out <- y + base::sample(resid, length(y),
        replace = TRUE)
      # !! important that length is based on y (because
      # of NAs) !!
    } else {
      boot <- base::sample(resid, length(y),
        replace = TRUE)
      boot_arma <- stats::arima.sim(arma_list,
        n = length(boot), innov = boot, n.start = 5,
        start.innov = rep(0, 5))
      out <- as.numeric(y + boot_arma)
    }
    return(out)
  }

  boot_fit <- function(y_boot, pr, t, model, dfF,
    v) {
    dat <- data.frame(pr = pr, y_boot = y_boot, t = t)
    if (class(model)[1] == "gam") {
      k <- dfF + 1
      family <- mgcv::summary.gam(model)$family[[1]]
      if (stringr::str_detect(family, "Negative Binomial")) {
      	 family <- "nb"
      }
      link <- mgcv::summary.gam(model)$family[[2]]
      dat$y_boot <- mod_y_boot(x = dat$y_boot, family = family)
      fit <- suppressWarnings(mgcv::gam(
      	 stats::as.formula(paste0("y_boot ~ s(pr, k = ",
        k, ", fx = TRUE)")), family = paste0(family,
        "(link = ", link, ")"), data = dat))
    } else {
      lmc <- nlme::lmeControl(niterEM = 5000,
        msMaxIter = 1000)
      k <- dfF + 1
      family <- mgcv::summary.gam(model$gam)$family[[1]]
      if (stringr::str_detect(family, "Negative Binomial")) {
      	 family <- "nb"
      }
      link <- mgcv::summary.gam(model$gam)$family[[2]]
      dat$y_boot <- mod_y_boot(x = dat$y_boot, family = family)
      pass_p <- attr(model$lme$modelStruct$corStruct,
        "p")
      pass_q <- attr(model$lme$modelStruct$corStruct,
        "q")
      fit <- suppressWarnings(mgcv::gamm(
      	 formula = stats::as.formula(paste0("y_boot ~ s(pr, k = ",
        k, ")")), family = paste0(family, "(link = ",
        link, ")"), correlation = nlme::corARMA(value = v,
        form = stats::as.formula("~t"), p = pass_p,
        q = pass_q), data = dat, control = lmc))
    }
    return(fit)
  }
  boot_fit_safe <- purrr::safely(boot_fit, otherwise = NA)


  # Wrapper function where above helper functions are
  # applied to each boot_id
  apply_boot <- function(x, n_bt) {

    boot_tbl <- tibble::tibble(boot_id = 1:n_bt)
    boot_tbl$press_seq <- rep(x$press_seq[1], n_bt)
    boot_tbl$xp <- rep(x$xp[1], n_bt)
    boot_tbl$xm <- rep(x$xm[1], n_bt)
    boot_tbl$delta <- rep(x$delta[1], n_bt)

    boot_tbl$y_boot <- vector("list", length = n_bt)
    for (i in seq_along(boot_tbl$boot_id)) {
      boot_tbl$y_boot[[i]] <- boot_y(y = x$ind_train[[1]],
        resid = x$resid[[1]], model = x$model[[1]],
        arma_list = x$arma_list[[1]])
    }

    boot_fit_l <- purrr::map(.x = boot_tbl$y_boot,
      .f = boot_fit_safe, pr = x$press_train[[1]],
      t = x$time_train[[1]], model = x$model[[1]],
      dfF = x$dfF[[1]], v = x$values[[1]]) %>%
    	 purrr::transpose()
    boot_tbl$boot_fit <- boot_fit_l %>% .$result
    boot_tbl$boot_each_error <- purrr::map(boot_fit_l$error, ~ .$message)


    # Here comes a loop to repeat the bootstrapping for
    # those cases where errors occurred (to get full
    # n_bt) -> but to avoid infinity loop set max to
    # 400 iterations
    for (i in seq_along(boot_tbl$boot_id)) {
      if (is.na(boot_tbl$boot_fit[i])) {
      	 m = 1
        repeat {
          temp_y_boot <- boot_y(y = x$ind_train[[1]],
          resid = x$resid[[1]], model = x$model[[1]],
          arma_list = x$arma_list[[1]])

          temp_boot_fit <- boot_fit_safe(y_boot = temp_y_boot,
          pr = x$press_train[[1]], t = x$time_train[[1]],
          model = x$model[[1]], dfF = x$dfF[[1]],
          v = x$values[[1]])
          m = m + 1
          if (!is.na(temp_boot_fit[1]) | m == 401) {
          break
          }
        }
        boot_tbl$boot_fit[[i]] <- temp_boot_fit$result
      }
    }

    # Calculate predicted values from boot_fit (with
    # ci's)
    boot_tbl$boot_pred <- calc_pred(model_list = boot_tbl$boot_fit,
      obs_press = boot_tbl$press_seq)$pred

    # Calculate derivatives
    boot_tbl$boot_xp <- calc_pred(model_list = boot_tbl$boot_fit,
      obs_press = boot_tbl$xp)$pred
    boot_tbl$boot_xm <- calc_pred(model_list = boot_tbl$boot_fit,
      obs_press = boot_tbl$xm)$pred

    boot_tbl$deriv1 <- purrr::pmap(.l = list(x = boot_tbl$boot_xp,
      y = boot_tbl$boot_xm, z = boot_tbl$delta),
      function(x, y, z) as.vector((x - y)/(2 *
        z)))
    # (pipe operator and formula do not work with pmap)

    return(boot_tbl)
  }

  apply_boot_safe <- purrr::safely(apply_boot, otherwise = NA)

  # -------

  # Apply apply_boot_safe() to every id
  boot_per_id <- vector(mode = "list", length = length(dat$id))

  if (par_comp == FALSE) {
    if (!is.null(seed)) {
    	 set.seed(seed)
    }
    # Apply the function in a loop now with progress
    # bar (most time consuming step)
    pb <- dplyr::progress_estimated(length(dat$id))
    for (i in seq_along(dat$id)) {
      boot_per_id[[i]] <- apply_boot_safe(x = dat[i, ],
        n_bt = n_boot)
      # Increment progress bar
      pb$tick()$print()
    }
    # Stop progress bar
    pb$stop()

  } else {  # parallel computing

    op <- pbapply::pboptions(type = "timer", char = "=")
    dat_list <- split(dat, dat$id)
    names(dat_list) <- NULL
    if (is.null(no_clust)) {
    	no_clust <- parallel::detectCores() - 1
    }
    cl <- parallel::makeCluster(no_clust, type = "PSOCK")
    parallel::clusterExport(cl, c("boot_y", "boot_fit",
      "mod_y_boot", "boot_fit_safe"), envir = environment())

    if (!is.null(seed)) {
      parallel::clusterEvalQ(cl, {
        library(INDperform)
        RNGkind()
      })
      parallel::clusterSetRNGStream(cl, seed)
    } else {
      parallel::clusterEvalQ(cl, {
        library(INDperform)
      })
    }

    doParallel::registerDoParallel(cl)
    boot_per_id <- pbapply::pblapply(dat_list,
      apply_boot_safe, cl = cl, n_bt = n_boot)
    parallel::stopCluster(cl)
    pbapply::pboptions(op)

 }

  # Group result and error sublists together
  boot_per_id_t <- boot_per_id %>% purrr::transpose()
  dat$boot_tbl <- boot_per_id_t$result
  dat$boot_error <- purrr::map(boot_per_id_t$result,	~.x$boot_each_error)

  # Check if number of successfull boots is acceptable
  n_succ <- purrr::map_dbl(dat$boot_tbl, ~ sum(!is.na(.x$boot_fit)))
  dat$adj_n_boot <- purrr::map_dbl(n_succ, .f = adj_n_boot)
  dat$boot_tbl <- purrr::map2(.x = dat$boot_tbl, .y = dat$adj_n_boot,
  	.f = sample_boot)


  # Calculate means and ci's for predicted and derivative values
  alp <- (1 - ci)/2

  dat$pred <- purrr::map(1:length(dat$boot_tbl),
    ~calc_value(input_list = dat$boot_tbl[.], var = "boot_pred",
      fun = mean, na.rm = TRUE))
  dat$pred_ci_up <- purrr::map(1:length(dat$boot_tbl),
    ~calc_value(input_list = dat$boot_tbl[.], var = "boot_pred",
      fun = calc_ci, z = 1 - alp, n = dat$adj_n_boot[.]))
  dat$pred_ci_low <- purrr::map(1:length(dat$boot_tbl),
    ~calc_value(input_list = dat$boot_tbl[.], var = "boot_pred",
      fun = calc_ci, z = alp, n = dat$adj_n_boot[.]))

  dat$deriv1 <- purrr::map(1:length(dat$boot_tbl),
    ~calc_value(input_list = dat$boot_tbl[.], var = "deriv1",
      fun = mean))
  dat$deriv1_ci_up <- purrr::map(1:length(dat$boot_tbl),
    ~calc_value(input_list = dat$boot_tbl[.], var = "deriv1",
      fun = calc_ci, z = 1 - alp, n = dat$adj_n_boot[.]))
  dat$deriv1_ci_low <- purrr::map(1:length(dat$boot_tbl),
    ~calc_value(input_list = dat$boot_tbl[.], var = "deriv1",
      fun = calc_ci, z = alp, n = dat$adj_n_boot[.]))


  # ------------------------------

  # Remove columns originating from init_tbl and generated here
  # which are not relevant, i.e.
  incl_var <- names(dat)[!names(dat) %in% c("ind_train",
  	"press_train", "time_train", "ind_test", "press_test",
  	 "time_test", "train_na", "delta", "xp", "xm", "dfF",
  	 "values", "resid", "arma_list", "boot_tbl")]
  out <- dat %>% dplyr::select_(.dots = incl_var)


   # Warning if some models were not fitted
	  if (any(is.na(out$adj_n_boot) |  out$adj_n_boot < n_boot)) {
					sel <- is.na(out$adj_n_boot) |  out$adj_n_boot < n_boot
					miss_mod <- out[sel, c(1:4, 20)] # would be here 19 but 'prop' included in calc_deriv before
				 message(paste0("NOTE: For the following IND~pressure GAMs bootstrapping fitting procedure ",
				 	"failed completely ('adj_n_boot' = NA) or partly so that the number of bootstraps ",
						"had to be reduced. See 'boot_error' in the output tibble for the error message of ",
						"each iteration. If 'adj_n_boot' = NA, try for these models the alternative ",
						"'approx_deriv' method:"))
	  	 print(miss_mod, n = Inf, tibble.width = Inf)
	  }

  ### END OF FUNCTION
  return(out)
}



# Internal helper functions -----------------------

# Function to check whether the selected bootstrap
# number matches with the ci_boot and adjust otherwise
check_n_boot <- function(n_boot, ci) {
  y <- (n_boot - (n_boot * ci))/2
  if (y != round(y)) {
    n_boot <- round(ceiling(y)/(1 - ci) * 2, digits = 0)
    message(paste("n_boot recalculated due to rounding issues. New n_boot =",
      n_boot, "."))
  }
  return(n_boot)
}

# Function to modify y_boot according to the family
# (predicted values and residuals always double format,
# which causes y_boot to be not in required integer or binary format)
mod_y_boot <- function(x, family) {
	if (family %in% c("poisson", "quasipoisson", "nb")) {
		out <- ceiling(x)
		out[out<0] <- 0
		out <- as.integer(out)
	} else {
		if (family %in% c("binomial")) {
			out <- rep(0, length(x))
			out[x >= 0.5] <- 1
		} else {
			out <- x
		}
	}
	return(out)
}


# Re-adjust the number of bootstrap iterations depending on
# the number of successfull model fits
# (if x is less than the min. required NA returned)
adj_n_boot <- function(x) {
		n_boot_seq <- seq(40, 120000, 40)
		y <- n_boot_seq[n_boot_seq <= x]
		if (length(y) == 0) y <- NA
		n_req <- max(y)
		return(n_req)
}

# Sample from the successfull bootstrap iterations based on
# the adjusted n_boot for calculating mean/ci of pred/derivs
sample_boot <- function(x, y) {
	# x: the boot_tbl to sample from
	# y: number of required iterations
	x$considered <- rep(FALSE, length(x$boot_fit))
	if (!is.na(y)) {
		 fit_succ <- which(!purrr::map_lgl(1:length(x$boot_fit),
		 ~ is.na(x$boot_fit[.])))
		 fit_sample <- sample(fit_succ, y)
	  x$considered[fit_sample] <- TRUE
	}
	return(x)
}

# Calculate means and ci
calc_ci <- function(x, z, n) {
	result <- sort(x)[n * z]
	return(result)
}

# To apply function like mean to lists
calc_value <- function(input_list, var, fun, ...) {
	if (is.na(input_list)) {
		result <- NA
	} else {
		sel <- input_list %>% purrr::flatten_dfr() %>% .$considered
		if (sum(sel) == 0) {
			result <- NA
		} else {
			x <- input_list %>% purrr::flatten_dfr() %>% .[[var]]
			x <- x[sel]
			result <- do.call(cbind, x) %>% apply(.,
				MARGIN = 1, FUN = fun, ...)
		}
	}
	return(result)
}
