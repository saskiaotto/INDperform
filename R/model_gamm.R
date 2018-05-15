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
#'  This needs to be defined as a family function (see also \code{\link{family}}).
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
		if (class(family) != "family") {
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


  # Loop with progress bar -------------------------
  # Initialise progress bar
  pb <- dplyr::progress_estimated(length(inputs))
  # Loop over input list
  for (i in seq_along(inputs)) {
    if (any(match == i)) {
      # Create ar0 GAMMs
      names(inputs[[i]])[1:2] <- dat[i, c("ind",
        "press")]
      gamms[[i]] <- suppressWarnings(try(mgcv::gamm(formula = stats::as.formula(paste0(names(inputs[[i]])[1],
        " ~ s(", names(inputs[[i]])[2], ", k = ",
        k, ")")), data = inputs[[i]], family = family), silent = TRUE))
      # Save original data as in model_gam
      gamms[[i]]$gam$train_na <- train_na_rep[[i]]
    } else {
    	 # Create GAMMs with corr structure
      names(inputs[[i]])[1:2] <- dat[i, c("ind",
        "press")]
      gamms[[i]] <- suppressWarnings(try(mgcv::gamm(formula = stats::as.formula(paste0(names(inputs[[i]])[1],
        " ~ s(", names(inputs[[i]])[2], ", k = ",
        k, ")")), correlation = nlme::corARMA(value = values[[i]],
        form = stats::as.formula("~time"),
        p = pass_p[i], q = pass_q[i]), data = inputs[[i]],
        family = family, control = lmc), silent = TRUE))
      try(gamms[[i]]$gam$train_na <- train_na_rep[[i]],
        silent = TRUE)
    }
    # Increment progress bar
    pb$tick()$print()
  }  # end of loop
  # Stop progress bar
  pb$stop()

  # Convert to dataframe with list columns and add
  # input data!
  gamm_tab <- tibble::tibble(id = temp$id, corrstruc = rep(c("none",
    "ar1", "ar2", "arma11", "arma12", "arma21"),
    length.out = length(gamms)), model = gamms) %>%
    dplyr::left_join(init_tbl[, 1:3], by = "id") %>%
    dplyr::arrange_(~id)

  # Report erroneous gamms
  choose <- purrr::map_chr(gamm_tab$model,
  	  ~class(.)[1]) != "try-error"
  no_fit <- gamm_tab[!choose, ]
  no_fit$model <- NULL
  if (nrow(no_fit) >= 1) {
    message("Warning: The following gamm models cannot be fitted:")
    print(as.data.frame(no_fit))
  }

  # Add mmodel_type
  gamm_tab$model_type <- "gamm"
  # Rearrange columns
  gamm_tab <- dplyr::select_(gamm_tab, .dots = c("id", "ind",
    "press", "model_type", "corrstruc", "model"))
  # Replace every 'try-error' with NA
  gamm_tab$model[!choose] <- NA

  # Save summary for each gamm
  gamm_summary <- purrr::map_if(gamm_tab$model,
    choose, ~mgcv::summary.gam(.$gam))

  # AIC for $gam is always numeric(0); get AIC from
  # lme object
  temp <- purrr::map_if(gamm_tab$model, choose, ~stats::AIC(.$lme))
  gamm_tab$aic <- unlist(temp)

  # Calculate nrmse using external helper function
  dummy <- dplyr::left_join(gamm_tab, init_tbl, by = c("press",
    "ind", "id"))
  tab <- calc_pred(model_list = dummy$model, obs_press = dummy$press_test)
  gamm_tab$nrmse <- calc_nrmse(pred = tab$pred, obs_ind = dummy$ind_test)

  # Get some summary output
  gamm_tab$edf <- get_sum_output(gamm_summary,
    varname = "edf")
  gamm_tab$r_sq <- get_sum_output(gamm_summary,
    varname = "r.sq")
  gamm_tab$p_val <- get_sum_output(gamm_summary,
    varname = "s.table", cell = 4)

  # Apply the significant code using external helper
  # function
  gamm_tab$signif_code <- get_signif_code(gamm_tab$p_val)

  # Get residuals
  res <- purrr::map_if(gamm_tab$model, choose, ~residuals(.$lme,
    type = "normalized"))

  # Test for normality in residual distribution
  suppressWarnings(temp <- purrr::map_if(res, choose, ~stats::ks.test(x = .,
    "pnorm", mean(.), sd(.))$p.value) )
  gamm_tab$ks_test <- round(unlist(temp), 4)

  # Test for TAC - need NA'S
  res_new <- vector(mode = "list", length = nrow(gamm_tab))
  for (i in seq_along(res_new)) {
    res_new[[i]] <- rep(NA, length(train_na_rep[[i]]))
    res_new[[i]][!train_na_rep[[i]]] <- res[[i]]
  }
  gamm_tab$tac <- test_tac(res_new)$tac

  # Check for outliers (cook's distance > 1) (in
  # residuals)
  cooks_dist <- purrr::map_if(gamm_tab$model, choose,
    ~cooks_dist_gamm(.$gam))
  warn <- purrr::map_lgl(cooks_dist, ~any(. > 1,
    na.rm = TRUE))
  gamm_tab$pres_outlier <- purrr::map2(cooks_dist,
    warn, ~if (.y == TRUE)
      which(.x > 1))
  gamm_tab$excl_outlier <- excl_outlier

  # Sort variables
  gamm_tab <- sort_output_tbl(gamm_tab)


  ### END OF FUNCTION
  return(gamm_tab)
}
