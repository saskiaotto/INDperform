#' Modelling of indicator responses to single pressures with GAMs
#'
#' \code{model_gam} applies Generalized Additive Models (GAMs) to each IND~pressure
#' combination created in \code{\link{ind_init}} and returns a tibble with
#' IND~pressure-specific GAM outputs.
#'
#' @param init_tbl The output tibble of the \code{\link{ind_init}} function.
#' @param k Choice of knots (for the smoothing function \code{\link{s}}); the default is 5.
#' @param family A description of the error distribution and link to be used in the GAM.
#'  This needs to be defined as a family function (see also \code{\link{family}}). All
#'  standard family functions can be used as well some of the distribution families in
#'  the mgcv package (see \code{\link[mgcv]{family.mgcv}}; e.g.\code{\link[mgcv]{negbin}}
#'  or \code{\link[mgcv]{nb}}).
#' @param excl_outlier A list of values identified as outliers in specific
#'  IND~pressure GAMs, which should be excluded in this modelling step
#'  (the output tibble of this function includes the variable
#'  'pres_outlier', which is a column-list containing
#'  all indices of values with cook's distance > 1 (see below). The function
#'  can be re-run again, then excluding all these outliers provided in
#'  \code{$pres_outlier} from the the first run (see example)).
#'
#' @details
#' To evaluate the IND's sensitivity and robustness time series of the IND are
#' modelled as a smoothing function of one single pressure variable (using a subset
#' of the data as training dataset, e.g. excluding the years of the annual time series).
#' The GAMs are build using the default settings in the \code{gam} function and
#' the smooth term function \code{\link[mgcv]{s}}).  However, the user can adjust
#' the distribution and link by modifying the family argument as well as the
#' maximum level of non-linearity by setting the number of knots:
#'
#' \code{gam(ind ~ s(press, k = k), family = family, data = training_data)}
#'
#' In the presence of significant temporal auto-correlation, GAMs should be extended to
#' Generalized Additive Mixed Models (GAMMs) by including auto-regressive error structures
#' to correct for the auto-correlation (Pinheiro and Bates, 2000). This is implemented in
#' the function \code{\link{model_gamm}}.
#'
#' The returned tibble contains various model outputs needed for scoring the sensitivity
#' and robustness subcriteria:
#' \itemize{
#'   \item \code{p_val} to identify whether an IND responds to a specific pressure
#'   \item \code{r_sq} for the strength of the IND response
#'   \item \code{edf} for the non-linearity of the IND response
#'   \item \code{nrmse} for the robustness of the established IND~pressure relationship
#' }
#'
#' The robustness of the modelled pressure relationship based on the training data
#' is evaluated by measuring how well the model prediction matches the test dataset,
#' e.g. the last years. This is quantified by computing the absolute value of the
#' normalised root mean square error (NRMSE) on the test dataset. The normalisation
#' to the mean of the observed test data allows for comparisons and a general scoring
#' of the model robustness across INDs with different scales or units.
#'
#'
#' @return
#' The function returns a \code{\link[tibble]{tibble}}, which is a trimmed down version of
#' the data.frame(), including the following elements:
#' \describe{
#'   \item{\code{id}}{Numerical IDs for the IND~press combinations.}
#'   \item{\code{ind}}{Indicator names.}
#'   \item{\code{press}}{Pressure names.}
#'   \item{\code{model_type}}{Specification of the modeltype; at this stage containing only
#'              "gam" (Generalized Additive Model).}
#'   \item{\code{corrstruc}}{Specification of the correlation structure; at this stage
#'              containing only "none".}
#'   \item{\code{aic}}{AIC of the fitted models}
#'   \item{\code{edf}}{Estimated degrees of freedom for the model terms.}
#'   \item{\code{p_val}}{The p values for the smoothing term (the pressure).}
#'   \item{\code{signif_code}}{The significance codes for the p-values.}
#'   \item{\code{r_sq}}{The adjusted r-squared for the models. Defined as the proportion
#'               of variance explained, where original variance and residual variance are
#'               both estimated using unbiased estimators. This quantity can be negative
#'               if your model is worse than a one parameter constant model, and can be
#'               higher for the smaller of two nested models.}
#'   \item{\code{expl_dev}}{The proportion of the null deviance explained by the models.}
#'   \item{\code{nrmse}}{Absolute values of the Normalized Root Mean Square Error (NRMSE).}
#'   \item{\code{ks_test}}{The p-values from a Kolmogorov-Smirnov Test applied on the model
#'               residuals to test for normal distribution. P-values > 0.05 indicate
#'               normally distributed residuals.}
#'   \item{\code{tac}}{logical; indicates whether temporal autocorrelation (TAC) was detected
#'               in the residuals. TRUE if model residuals show TAC. NAs in the time series
#'               due to real missing values, test data extraction or exlusion of outliers
#'               are explicitely considered. The test is based on the following condition:
#'               if any of the acf \strong{and} pacf values of lag 1 - 5 are greater than 0.4
#'               or lower than -0.4, a TRUE is returned.}
#'   \item{\code{pres_outlier}}{A list-column with all indices of values identified as outliers
#'               in each model (i.e.cook's distance > 1).}
#'   \item{\code{excl_outlier}}{A list-column listing all outliers per model that have been
#'               excluded in the GAM fitting}
#'   \item{\code{model}}{A list-column of IND~press-specific gam objects that contain additionally
#'              the logical vector indicating missing values (\code{$train_na}).}
#' }
#'
#' @references
#' Pinheiro, J.C., Bates, D.M. (2000) Mixed-Effects Models in S and S-Plus.
#' Springer, New York, 548pp.
#'
#' @seealso \code{\link[tibble]{tibble}} and the \code{vignette("tibble")} for more
#'  informations on tibbles,
#'  \code{\link[mgcv]{gam}} for more information on GAMs, and
#'  \code{\link{plot_diagnostics}} for assessing the model diagnostics
#' @family IND~pressure modelling functions
#'
#' @export
#'
#' @examples
#' # Using the Baltic Sea demo data in this package
#' dat_init <- ind_init(
#'   ind_tbl = ind_ex[, c("Sprat", "Cod")],
#'   press_tbl = press_ex[, c("Tsum", "Swin", "Fcod", "Fher")],
#'   time = ind_ex[ ,1])
#' gam_tbl <- model_gam(dat_init)
#' # Any outlier?
#' gam_tbl$pres_outlier
#' # Exclude outliers by passing this list as input:
#' gam_tbl_out <- model_gam(dat_init, excl_outlier = gam_tbl$pres_outlier)
#'
#' \dontrun{
#'  # Using another error distribution
#'  ind_sub <- round(exp(ind_ex[ ,c(2,8,9)]),0) # to unlog data and convert to integers
#'  ind_tbl2 <- ind_init(ind_sub, press_ex, time = ind_ex$Year)
#'  model_gam(ind_tbl2, family = poisson(link="log"))
#' }
model_gam <- function(init_tbl, k = 5, family = stats::gaussian(),
  excl_outlier = NULL) {

		# Data input validation -----------------------
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

	 # ----------------

	 # Exclude outliers given in excl_outliers
  if (!is.null(excl_outlier)) {
    ind_train_sub <- purrr::map2(init_tbl$ind_train,
      excl_outlier, ~replace(.x, .y, NA))  # (NULLs will not be replaced)
    press_train_sub <- purrr::map2(init_tbl$press_train,
      excl_outlier, ~replace(.x, .y, NA))
    # consider excluded outliers also as NAs (=TRUE) in
    # the train_na vector (if randomly selected test
    # data, the correct years need to be chosen):
    train_na <- init_tbl$train_na
    for (i in seq_along(excl_outlier)) {
      if (!is.null(excl_outlier[[i]])) {
        train_na[[i]][match(init_tbl$time_train[[i]][excl_outlier[[i]]],
          as.numeric(names(init_tbl$train_na[[i]])))] <- TRUE
      }
    }
  } else {
    ind_train_sub <- init_tbl$ind_train
    press_train_sub <- init_tbl$press_train
    train_na <- init_tbl$train_na
  }

  gam_tab <- init_tbl[, 1:3]
  # Add gamm relevant columns for later merges
  gam_tab$model_type <- "gam"
  gam_tab$corrstruc <- "none"
  # Add models
  gam_tab$model <- vector(length = nrow(init_tbl),
    mode = "list")

  # Compute the IND~Press GAMs using this helper function:
  apply_gam <- function(ind_name, press_name,ind_ts,
  	 press_ts, train_na_ts, k, family) {
    dat <- data.frame(ind = ind_ts, press = press_ts)
    names(dat) <- c(ind_name, press_name)
    model <- mgcv::gam(stats::as.formula(paste0(names(dat)[1],
      " ~ 1 + s(", names(dat)[2], ", k = ", k,
      ")")), na.action = "na.omit", family = family, data = dat)
    # Save train_na in model
    model$train_na <- train_na_ts
    return(model)
  }
  apply_gam_safe <- purrr::safely(apply_gam, otherwise = NA)

  temp_mod <- purrr::pmap(.l = list(ind_name = init_tbl$ind,
	  	press_name = init_tbl$press, ind_ts = ind_train_sub,
	    press_ts = press_train_sub, train_na_ts = train_na),
	    .f = apply_gam_safe, k = k, family = family) %>%
  	 purrr::transpose()
  gam_tab$model <-	temp_mod$result

  if (all(is.na(temp_mod$result))) {
  	 stop("No IND~pressure GAM could be fitted! Check if you chose the correct error distribution (default is 'gaussian()').")
  } else {

  # Save summary for each gam (cannot handle NAs, hence use of possibly())
  gam_smy <- suppressWarnings(gam_tab$model %>%
  		purrr::map(.f = purrr::possibly(mgcv::summary.gam, NA_real_)))

  # Get some output from the summary
  gam_tab$edf <- get_sum_output(sum_list = gam_smy,
    varname = "edf")
  gam_tab$r_sq <- get_sum_output(sum_list = gam_smy,
    varname = "r.sq")
  gam_tab$expl_dev <- get_sum_output(sum_list = gam_smy,
    varname = "dev.expl")
  gam_tab$p_val <- get_sum_output(sum_list = gam_smy,
    varname = "s.table", cell = 4)

  # Apply the significant code using external helper function
  gam_tab$signif_code <- get_signif_code(gam_tab$p_val)

  # Save AIC value for each gam (cannot handle NAs, hence use of possibly())
  gam_tab$aic <- gam_tab$model %>% purrr::map_dbl(.f = purrr::possibly(stats::AIC, NA_real_))

  # Calculate nrmse using external helper function
  gam_tab$nrmse <- calc_nrmse(
  	 pred = calc_pred(model_list = gam_tab$model,
      obs_press = init_tbl$press_test)$pred,
  	 obs_ind = init_tbl$ind_test)

  # Get residuals (cannot handle NAs, hence use of possibly())
  res <- gam_tab$model %>% purrr::map(
  	 .f = purrr::possibly(mgcv::residuals.gam, NA_real_), type = "deviance")

  # Test for normality in residual distribution
  # (requires self-made safely function if res = NA)
  norm_test <- function(x, family) {
  	 p <- stats::ks.test(x = x, "pnorm", mean(x, na.rm = TRUE),
  	 	 stats::sd(x, na.rm = TRUE))$p.value
  	 return(p)
  }
  norm_test_safe <- purrr::safely(norm_test, otherwise = NA_real_)
  suppressWarnings(gam_tab$ks_test <- res %>%
  		purrr::map(.f = norm_test_safe) %>%
  	 purrr::transpose() %>% .$result %>% unlist() %>% round(., 4))

  # Test for TAC - needs NA's in residuals!
  res_new <- vector(mode = "list", length = nrow(init_tbl))
  for (i in seq_along(res_new)) {
    res_new[[i]] <- rep(NA, length(train_na[[i]]))
    res_new[[i]][!train_na[[i]]] <- res[[i]]
  }
  gam_tab$tac <- test_tac(res_new)$tac

  # Check for outlier (cook's distance > 1) in residuals
  #  (cannot handle NAs, hence use of possibly())
  cooks_dist <- purrr::map(gam_tab$model,
  	 .f = purrr::possibly(stats::cooks.distance, NA))
  warn <- purrr::map_lgl(cooks_dist, ~any(. > 1,
    na.rm = TRUE))
  outlier <- purrr::map2(warn, cooks_dist,
    ~if (.x == TRUE) which(.y > 1))
  # Get correct position of outlier in ind_train (important
  # if NAs present) and save in tibble
  gam_tab$pres_outlier <- purrr::map(1: length(ind_train_sub),
				~if (warn[[.]] == TRUE)  which(!is.na(ind_train_sub[[.]]))[outlier[[.]]])
  gam_tab$excl_outlier <- excl_outlier

  # end of if-statement (temp_mod$result not NA)
 	}

  # Sort variables
  gam_tab <- sort_output_tbl(gam_tab)
  # Sort rows
  gam_tab <- dplyr::arrange_(gam_tab, .dots = "id")

  # Warning if some models were not fitted
  if (any(!purrr::map_lgl(temp_mod$error, .f = is.null))) {
  	 sel <- !purrr::map_lgl(temp_mod$error, .f = is.null)
			 miss_mod <- gam_tab[sel, 1:3]
			 miss_mod$error_message <- purrr::map(temp_mod$error, .f = as.character) %>%
			 	 purrr::flatten_chr()
			 message("NOTE: For the following IND~pressure GAMs fitting procedure failed:")
  	 print(miss_mod)
  }


  ### END OF FUNCTION
  return(gam_tab)
}
