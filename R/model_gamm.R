#' Modelling of indicator responses to single pressures with GAMMs
#'
#' \code{model_gamm} accounts for temporal autocorrelation (TAC) in the time series
#' by fitting Generalized Additive Mixed Models (GAMMs) that include AR or ARMA
#' correlation structures using the \code{\link[mgcv]{gamm}} function. The GAMMs
#' are applied to all IND~pressure combinations provided as input or only those
#' with significant TAC in the GAM residuals (using filter argument).
#'
#' @param init_tbl The output tibble of the \code{\link{ind_init}} function.
#' @param k Choice of knots (for the smoothing function \code{\link{s}}); the
#'  default is 5.
#' @param family A description of the error distribution and link to be used in the GAM.
#'  This needs to be defined as a family function (see also \code{\link{family}}). All
#'  standard family functions can be used as well some of the distribution families in
#'  the mgcv package (see \code{\link[mgcv]{family.mgcv}}; e.g.\code{\link[mgcv]{negbin}}).
#'  Note that \code{\link[mgcv]{nb}}, which estimates \code{theta} parameter, cannot be used
#'  for \code{\link[mgcv]{gamm}}.
#' @param excl_outlier A list of values identified as outliers in specific
#'  IND~pressure GAMMs, which should be excluded in this modelling step
#'  (the output tibble of this function includes the variable
#'  'pres_outlier', which is a column-list containing
#'  all indices of values with cook's distance > 1 (see below). The function
#'  can be re-run again, then excluding all these outliers provided in
#'  \code{$pres_outlier} from the the first run (see example)).
#' @param filter logical; a filter used to select specific rows in init_tbl
#'  (row gets selected if value TRUE). That could be the \code{tac} column
#'  in the \code{model_gam} output tibble which indicates whether the model
#'  residuals show TAC.
#'
#' @details
#' Modelling first-differenced indicator time series can be an alternative solution
#' to avoid temporal dependence between observations. However, this approach does
#' often not help reducing the significant auto-correlation while GAMMs do as found in
#' Otto \emph{et al.} (2018). Such an extension implies that the single
#' elements of the response variable are not independent anymore and that the
#' correlation between the residuals at time t1 and t2 only depends on their time
#' difference t1 – t2 (Wood, 2006).
#'
#' In \code{model_gamm} six GAMMs are computed for each filtered IND~pressure pair, i.e.
#' \enumerate{
#'   \item no correlation structure (for AIC comparison)
#'   \item auto-regressive error structure of order p=1 (AR1)
#'   \item auto-regressive error structure of order p=2 (AR2)
#'   \item auto-regressive moving average of order p=1 and q=1 (ARMA11)
#'   \item auto-regressive moving average of order p=1 and q=2 (ARMA12)
#'   \item auto-regressive moving average of order p=2 and q=1 (ARMA21)
#' }
#'
#' @return
#' Returns a model output tibble that contains for each filtered IND~pressure pair
#' 6 rows with the individual GAMM outputs. The structure remains the same as in
#' \code{\link{model_gam}} except for the explained deviance, which is not computed
#' by the gamm function.
#' The selection of the final correlation structure for each IND~pressure model can be
#' done manually on this tibble or with an automatized routine using
#' \code{\link{select_model}}.
#'
#' @references
#' Otto, S.A., Kadin, M., Casini, M., Torres, M.A., Blenckner, T. (2018)
#' A quantitative framework for selecting and validating food web indicators.
#' \emph{Ecological Indicators}, 84: 619-631,
#' doi: https://doi.org/10.1016/j.ecolind.2017.05.045
#'
#' Wood, S.N. (2006) Generalized Additive Models: An Introduction with R.
#' Chapman and Hall/CRC Press
#'
#' @seealso \code{\link[mgcv]{gamm}} for more information on GAMMs and
#' \code{\link{plot_diagnostics}} for assessing the model diagnostics
#' @family IND~pressure modelling functions
#'
#' @export
#'
#' @examples
#' # Using the Baltic Sea demo data in this package
#' dat_init <- ind_init(
#'   ind_tbl = data.frame(Cod = ind_ex$Sprat),
#'   press_tbl = press_ex[, c("Fsprat", "Fher")],
#'   time = ind_ex[ ,1])
#' gam_tbl <- model_gam(dat_init)
#' # Any temporal autocorrelation
#' gam_tbl$tac
#' # Applying model_gamm function and passing the $tac variable as filter
#' gamm_tbl <- model_gamm(dat_init, filter = gam_tbl$tac)
model_gamm <- function(init_tbl, k = 5, family = stats::gaussian(),
  excl_outlier = NULL, filter = NULL) {
	# init_tbl = dat_init

  # Data input validation ---------------------
		if (missing(init_tbl)) {
	 	stop("Argument 'init_tbl' is missing.")
	 }
		# Check input tibble
		init_tbl <- check_input_tbl(
				init_tbl, tbl_name = "init_tbl", parent_func = "ind_init()",
				var_to_check = c("id", "ind", "press", "ind_train", "press_train", "time_train",
					"ind_test", "press_test", "time_test", "train_na"),
				dt_to_check = c("integer", "character", "character", rep("list", 7))
		)

		# Check family class
		if (!"family" %in% class(family)) {
				stop("The specified family is not a family object. You need to provide the family function, e.g. family = poisson()")
		}

  # Check that excl_outlier list has the correct length
  # (i.e. 6 times the id: 1 for each GAMM)
  if (!is.null(excl_outlier)) {
    if (length(excl_outlier) != 6 * nrow(init_tbl)) {
      stop(paste0("The list of outliers to exclude in each GAMM",
        " has not the correct length.", " It should be a sixfold (6 GAMMs are computed) of all ids,",
        " so the required length is: ", 6 *
          nrow(init_tbl)))
    }
  }

		# Filter for models with tac only
  if (!is.null(filter)) {
  		if (length(filter) != nrow(init_tbl)) {
  			 stop("The length of the logical 'filter' vector deviates from the number of rows in 'ind_init'!")
  		} else {
  			 init_tbl <- init_tbl[filter, ]
  		}
  }

  # Create input list -------------------------------
  # Lengthen init_tbl to create 6 models
  temp <- tibble::tibble(id = rep(init_tbl$id, each = 6))
  dat <- dplyr::left_join(temp, init_tbl)
  inputs <- purrr::map(1:nrow(dat),
  	 ~tibble::tibble(ind = dat[[., "ind_train"]],
  	 	  press = dat[[., "press_train"]],
       time = dat[[., "time_train"]]))
  # Replicate the train_na vector for testing for tac
  train_na_rep <- rep(init_tbl$train_na, each = 6)
  names(train_na_rep) <- rep(names(init_tbl$train_na),
    each = 6)

  # Exclude outliers given in excl_outliers
  if (!is.null(excl_outlier)) {
    ind <- purrr::map2(inputs, excl_outlier,
    	~replace(.x$ind,
      .y, NA))  # (all NULLs will not be replaced)
    press <- purrr::map2(inputs, excl_outlier,
      ~replace(.x$press, .y, NA))  # (NULLs will not be replaced)
    time <- purrr::map2(inputs, excl_outlier,
    	 ~replace(.x$time, .y, NA))  # (NULLs will not be replaced)
    inputs <- purrr::pmap(.l = list(ind = ind,
      press = press, time = time), function(ind,
      press, time) tibble::tibble(ind = ind,
      press = press, time = time))

    # Consider excluded outliers also as NAs (=TRUE) in
    # the train_na_rep vector (if randomly selected
    # test data, the correct years need to be chosen).
    # Replicate the other vectors first as well
    time_train_rep <- rep(init_tbl$time_train,
      each = 6)
    for (i in seq_along(excl_outlier)) {
      if (!is.null(excl_outlier[[i]])) {
        train_na_rep[[i]][match(time_train_rep[[i]][excl_outlier[[i]]],
          as.numeric(names(train_na_rep[[i]])))] <- TRUE
      }
    }
  }

  # Fit GAMMs -------------------------------
  # Set control parameters
  lmc <- nlme::lmeControl(niterEM = 5000, msMaxIter = 1000)
  # Generate starting values for corr structure
  pass_p <- c(NA, 1, 2, 1, 1, 2)
  pass_p <- rep(pass_p, length.out = length(inputs))
  pass_q <- c(NA, 0, 0, 1, 2, 1)
  pass_q <- rep(pass_q, length.out = length(inputs))
  values <- list(NA, c(0.3), c(0.3, -0.3), c(0.3,
    0.3), c(0.3, -0.3, 0.3), c(0.3, 0.3, -0.3))
  values <- rep(values, length.out = length(inputs))
  # Create model list
  gamms <- vector(mode = "list", length = length(pass_p))
  match <- seq(from = 1, to = length(inputs), by = 6)


  # self-made GAMM function to capture side-effects (error messages)
  gamm_ar0_func <- function(x, y, data, family){
  	form <- stats::as.formula(paste0(y, " ~ s(", x, ", k = ",
  		k, ")"))
  	temp_mod <- mgcv::gamm(formula = form, data = data, family = family)
  	return(temp_mod)
  }
  gamm_ar0_func_safe <- purrr::safely(gamm_ar0_func, otherwise = NA)

  gamm_ar_func <- function(x, y, data, family, ar_values, p, q,
  	control){
  	form <- stats::as.formula(paste0(y, " ~ s(", x, ", k = ",
  		k, ")"))
  	temp_mod <- mgcv::gamm(formula = form, data = data, family = family,
  		correlation = nlme::corARMA(value = ar_values,
  			form = stats::as.formula("~time"), p = p, q = q),
  		control = control)
  	return(temp_mod)
  }
  gamm_ar_func_safe <- purrr::safely(gamm_ar_func, otherwise = NA)


  # Loop with progress bar -------------------------
  # Initialise progress bar
  pb <- dplyr::progress_estimated(length(inputs))
  # Loop over input list
  for (i in seq_along(inputs)) {
    if (any(match == i)) {
      # Create ar0 GAMMs
      names(inputs[[i]])[1:2] <- dat[i, c("ind",
        "press")]
      gamms[[i]] <- suppressWarnings(
      	gamm_ar0_func_safe(x = names(inputs[[i]])[2], y = names(inputs[[i]])[1],
      		data = inputs[[i]], family = family))
      # Save original data in model_gam if fitting successfull
      if (is.null(gamms[[i]]$error)) {
      	 gamms[[i]]$result$gam$train_na <- train_na_rep[[i]]
      }
    } else {
    	 # Create GAMMs with corr structure
      names(inputs[[i]])[1:2] <- dat[i, c("ind",
        "press")]
      gamms[[i]] <- suppressWarnings(
      	gamm_ar_func_safe(x = names(inputs[[i]])[2], y = names(inputs[[i]])[1],
      		data = inputs[[i]], family = family, ar_values =values[[i]],
      		p = pass_p[i], q = pass_q[i],control = lmc))
      if (is.null(gamms[[i]]$error)) {
      	 gamms[[i]]$result$gam$train_na <- train_na_rep[[i]]
      }
    }
    # Increment progress bar
    pb$tick()$print()
  }  # end of loop
  # Stop progress bar
  pb$stop()

  # Transpose gamm list
  temp_mod <- gamms %>%	purrr::transpose()

  if (all(is.na(temp_mod$result))) {
  	stop("No IND~pressure GAMM could be fitted! Check if you chose the correct error distribution (default is 'gaussian()').")
  } else {

  	# Convert to dataframe with list columns and add
  	# input data!
  	gamm_tab <- tibble::tibble(id = temp$id,
  		corrstruc = rep(c("none",
  			"ar1", "ar2", "arma11", "arma12", "arma21"),
  			length.out = length(temp_mod$result)),
  		model = temp_mod$result) %>%
  		dplyr::left_join(init_tbl[, 1:3], by = "id") %>%
  		dplyr::arrange_(~id)

  	# Add model_type
  	gamm_tab$model_type <- "gamm"
  	# Rearrange columns
  	gamm_tab <- dplyr::select_(gamm_tab, .dots = c("id", "ind",
  		"press", "model_type", "corrstruc", "model"))

  	# Save summary for each gam (cannot handle NAs, hence use of possibly())
  	summary_gam_safe <- purrr::possibly(mgcv::summary.gam, NA_real_)
  	gamm_smy <- suppressWarnings(purrr::map(gamm_tab$model,
    ~summary_gam_safe(.$gam)))

  	# Get some output from the summary
  	gamm_tab$edf <- get_sum_output(sum_list = gamm_smy,
  		varname = "edf")
  	gamm_tab$r_sq <- get_sum_output(sum_list = gamm_smy,
  		varname = "r.sq")
  	gamm_tab$p_val <- get_sum_output(sum_list = gamm_smy,
  		varname = "s.table", cell = 4)

  	# Apply the significant code using external helper
  	# function
  	gamm_tab$signif_code <- get_signif_code(gamm_tab$p_val)

  	# Save AIC value for each gam (cannot handle NAs, hence use of possibly())
  	aic_safe <- purrr::possibly(stats::AIC, NA_real_)
  	gamm_tab$aic <- gamm_tab$model %>% purrr::map_dbl(~aic_safe(.$lme))

  	# Calculate nrmse using external helper function
  	dummy <- dplyr::left_join(gamm_tab, init_tbl, by = c("press",
  		"ind", "id"))
  	gamm_tab$nrmse <- calc_nrmse(
  		pred = calc_pred(model_list = dummy$model,
  			obs_press = dummy$press_test)$pred,
  		obs_ind = dummy$ind_test)

  	# Get residuals (cannot handle NAs, hence use of possibly())
  	choose <- purrr::map_lgl(temp_mod$error, .f = is.null)
  	res <- purrr::map_if(gamm_tab$model, choose, ~residuals(.$lme,
    type = "normalized"))

  	# Test for normality in residual distribution
  	# (requires self-made safely function if res = NA)
  	norm_test <- function(x, family) {
  		p <- stats::ks.test(x = x, "pnorm", mean(x, na.rm = TRUE),
  			stats::sd(x, na.rm = TRUE))$p.value
  		return(p)
  	}
  	norm_test_safe <- purrr::safely(norm_test, otherwise = NA_real_)
  	suppressWarnings(gamm_tab$ks_test <- res %>%
  			purrr::map(.f = norm_test_safe) %>%
  			purrr::transpose() %>% .$result %>% unlist() %>% round(., 4))

  	# Test for TAC - needs NA's in residuals!
  	res_new <- vector(mode = "list", length = nrow(gamm_tab))
  	for (i in seq_along(res_new)) {
  		res_new[[i]] <- rep(NA, length(train_na_rep[[i]]))
  		res_new[[i]][!train_na_rep[[i]]] <- res[[i]]
  	}
  	gamm_tab$tac <- test_tac(res_new)$tac

  	# Check for outlier (cook's distance > 1) in residuals
  	#  (cannot handle NAs, hence use of possibly())
  	cooks_dist <- purrr::map(gamm_tab$model,
  		.f = purrr::possibly(stats::cooks.distance, NA))
  	warn <- purrr::map_lgl(cooks_dist, ~any(. > 1,
  		na.rm = TRUE))
  	gamm_tab$pres_outlier <- purrr::map2(cooks_dist,
  		warn, ~if (.y == TRUE)
  			which(.x > 1))
  	gamm_tab$excl_outlier <- excl_outlier
  }

  # Sort variables
  gamm_tab <- sort_output_tbl(gamm_tab)

  # Warning if some models were not fitted
  if (any(!purrr::map_lgl(temp_mod$error, .f = is.null))) {
  	 sel <- !purrr::map_lgl(temp_mod$error, .f = is.null)
			 miss_mod <- gamm_tab[sel, 1:5]
			 miss_mod$error_message <- purrr::map(temp_mod$error, .f = as.character) %>%
			 	 purrr::flatten_chr()
			 message("NOTE: For the following IND~pressure GAMMs fitting procedure failed:")
  	 print(miss_mod)
  }

  ### END OF FUNCTION
  return(gamm_tab)
}
