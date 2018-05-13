#' Calculate the derivatives of the IND response functions and the proportion
#' of pressure range in which the response is significant
#'
#' In the case of non-linear IND responses to pressures first derivatives
#' of the response curve are calculated to identify whether the IND ceases
#' to respond at the lower or upper end of the pressure range (relevant for
#' sub-crit. 9.2). \code{calc_deriv} serves as a wrapper function that filters
#' first the model input tibble and applies as default method the 'conditional
#' bootstrap' (see the function \code{\link{cond_boot}}). It calculates from
#' the computed bootstrapped derivatives and confidence intervals the
#' proportion of pressure range in which the IND shows a significant change.
#' The implementation of the 'conditional bootstrap' for Generalized Additive
#' Mixed Models (GAMMs) has some caveats (see \emph{Details}), which is why
#' we implemented additionally a rather quick-and-dirty approach (through the
#' \code{\link{approx_deriv}} function). In the 'approx_deriv' method derivatives
#' are calculated directly from the smoothing curve of the original method. The
#' confidence intervals are then just approximated from the standard errors of the
#' smoothing curve. For more informations on this method see
#' \code{\link{approx_deriv}}.
#'

#' @param init_tbl The output tibble of the \code{\link{ind_init}} function.
#' @param mod_tbl A model output tibble from \code{\link{model_gam}},
#'  \code{\link{select_model}} or \code{\link{merge_models}} representing the
#'  best model for each IND~pressure pair.
#' @param edf_filter The minimum edf value at which derivatives are calculated
#'  for the respective model. The default is set to 1.5.
#' @param p_val_filter Significance level for selecting models on which to calculate
#'  the derivatives. Only models with a p value smaller than the p_val_filter will be
#'  selected; the default is 0.05.
#' @param excl_outlier logical; if TRUE, the outliers excluded in the original
#'  models will be also excluded in the bootstrapped models.
#' @param method Method for calculating derivatives and CI; can be either 'conditional
#'  bootstrap' (default) or 'approx_deriv'.
#' @param n_boot Number of bootstraps. Select n_boot so that (n_boot - (n_boot *ci)) / 2
#'  will be an integer. Otherwise, the function will increase n_boot automatically.
#'  The default is set to 200.
#' @param ci_boot Confidence interval of the boostrapped smoothing functions and their
#'  derivatives. Must be between 0 and 1, default is 0.95.
#' @param ci_prop_se A conversion factor for approximating derivative CIs in the
#'  'approx_method'; it is multiplied with the ratio between s.e. and mean fitted
#'  values of the smoothing curve to represent some level of uncertainty around the
#'  slope proportional to the uncertainty in the smoothing curve. Default is 25,
#'  which is a compromise representing fairly well the results obtained for the GAMs
#'  from the conditional bootstrap.
#' @param par_comp logical; if TRUE, the conditional bootstrap will be processed in
#'  parallel using several clusters, which can speed up the iteration process depending
#'  on the number of n_boot, models to bootstrap and number of processor cores.
#' @param no_clust Number of clusters ("workers") for the parallel computation, with one
#'   cluster per core. If no_clust is set to NULL default, the number of clusters is set
#'   as the numbers of available cores – 1.
#' @param seed A single value, interpreted as an integer, which specifies the seed
#'   of the random number generator (RNG) state for reproducibility. Due to the work
#'   splitting in the parallel computation, RNG streams are not comparable with the
#'   stream under serial computation. To reproduce results use the same type of
#'   computation with the same seed and no_clust.
#'
#' @details
#' In the case of non-linear IND responses to pressures first derivatives
#' of the response curve are calculated to identify whether the IND ceases
#' to respond at the lower or upper end of the pressure range (relevant for
#' sub-crit. 9.2).
#'
#' \strong{First Derivative:}
#'
#' The first derivative of a smoothing function i.e. s′(x),
#' represents the instantaneous rate of change of a dependent variable y to
#' that of the independent variable x. It tells us whether a function is
#' increasing or decreasing, and by how much. This information is reflected by
#' the slope of the tangent line to a point x on the graph of a function.
#' A positive slope tells us that, as x increases, s(x) also increases.
#' Derivatives can be applied to any function, even one as potentially
#' complex as a fitted spline function, by using the method of finite
#' differences (Trenkel and Rochet, 2009) as done in the \code{\link{cond_boot}}
#' function: The local first derivatives of the fitted GAM(M) time series
#' is estimated as the difference between fitted values at time step t and
#' t + d divided by d, where d is a small value, e.g. 1E-7.
#'
#' \strong{Confidence intervals (CI) based on a conditional bootstrap:}
#'
#' By calculating approximate confidence intervals for s′(x), it may
#' be determined whether or not apparent IND changes are statistically
#' significant, i.e. whether the non-zero estimate obtained for
#' the slope is distinguishable from zero given the uncertainty in the
#' estimate (Fewster et al., 2000). To estimate the CI of
#' the estimated first derivative, a conditional bootstrap is
#' carried out by resampling from the GAM residuals (in the
#' \code{\link{cond_boot}} function) (Large \emph{et al.}, 2013).
#' This approach has the advantage to retain the information in the
#' pressure variable and to be distribution-free.
#'
#' In specific, new IND time series are created in \code{\link{cond_boot}} by
#' resampling from the residuals of the original IND-Pressure GAM(M) and
#' adding these to the original IND time series repeatedly (the number of
#' repetitions is defined by n_boot).
#' A separate GAM(M) is then fitted to each bootstrap IND
#' time series in \code{\link{cond_boot}}, using the same effective degrees
#' of freedom (edf) as found optimal for the original IND time series. For
#' each of the bootstrapped GAMs, predicted IND time series given an evenly
#' spaced pressure sequence are produced for which the first derivatives
#' are calculated. Confidence intervals are computed by sorting the
#' bootstrapped derivatives into ascending order and calculating the
#' upper and lower percentiles defined by the \code{ci} argument (the
#' default is the 2.5\% and 97.5\% percentiles representing the 95\% CI).
#'
#' A problem with GAMMs is that the smoothing parameters cannot be fixed,
#' because the \code{\link[mgcv]{gamm}} function will treat the wiggly
#' components of the smooth terms as random effects, the variance of which
#' to be estimated by lme. Hence, the GAMMs refitted on the bootstrapped IND
#' time series are allowed to have every shape from linear to the max. edfs
#' set originally. This leads to often positive as well as negative linear
#' smoothers, which increases the confidence intervals of both fitted
#' smoothers as well as derivative. When the IND response is weak and/or
#' the error around the smoother high, the implemented routine to estimate
#' the proportion of pressure range with significant slope can lead to 0\%.
#' For those models, the method 'approx_deriv' can be applied for comparison.
#'
#' \strong{Calculating the proportion of pressure range:}
#'
#' The lack of any further IND response to pressure changes at
#' its minimum or maximum measured is noted when zero is contained
#' within the CI of the first derivative and quantified by calculating the
#' proportion of points (evenly distributed along the pressure axis)
#' inside the CI for scoring criteria 10.2. In specific,
#' \code{calc_deriv} implements a routine, which identifies
#' first which pressure values has a slope of zero (see the returned
#' list-column \code{zero_in_conf}). It then checks whether the first
#' value in \code{zero_in_conf} is TRUE (i.e., the lowest value of the
#' evenly spaced pressure sequence has zero slope). If so, the first value
#' in the vector zic_start_end (see the returned list-column
#' \code{zic_start_end}) is also set to TRUE, meaning it goes into
#' the proportion calculation as no IND response. The routine proceeds
#' to the next values \code{zero_in_conf} and stops at the first FALSE value.
#' It then starts the same procedure from end of the \code{zero_in_conf}
#' vector. The proportion of pressure range (returned as \code{prop}
#' variable in the output tibble) reflects the proportion of FALSE values
#' in \code{zero_in_conf}.
#'
#' @return
#' The function returns the input model tibble with the following 10 columns added
#' \describe{
#'   \item{\code{prop}}{The proportion of the observed pressure range where the IND
#'              indicator shows a response (see the last section in \emph{Details}).
#'              Significant models with edf < edf_filter get as default a value
#'              of 1.0 (i.e., the IND responses to the entire observed pressure
#'              range).}
#'   \item{\code{zero_in_conf}}{A list-column of logical vectors indicating for
#'              every pressure value (in press_seq) whether the slope of the IND
#'              response at that pressure value is within the confidence interval,
#'              i.e. is zero.}
#'   \item{\code{zic_start_end}}{A list-column of logical vectors indicating for
#'              every pressure value (in press_seq) whether the slope is considered
#'              as zero for the proportion calculation (see the last section in
#'              \emph{Details}).}
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
#' }
#' If none of the significant models has edf > edf_filter, only the variable \code{prop}
#' will be added. If the 'approx_deriv' method was used, the output tibble will
#' not contain the \code{pred}, \code{pred_ci_up}, and \code{pred_ci_low} variables.
#'
#' @references
#' Fewster, R.M., Buckland, S.T., Siriwardena, G.M., Baillie, S.R., Wilson, J.D. (2000)
#' Analysis of population trends for farmland birds using generalized additive models.
#' \emph{Ecology} 81, 1970-1984.
#'
#' Large, S.I., Fay, G., Friedland, K.D., Link, J.S. (2013) Defining trends and thresholds in
#' responses of ecological indicators to fishing and environmental pressures.
#' \emph{ICES Journal of Marine Science} 70, 755-767.
#'
#' Trenkel, V.M., Rochet, M.J. (2009) Intersection-union tests for characterising recent
#' changes in smoothed indicator time series. \emph{Ecological Indicators} 9, 732-739.
#'
#' @seealso \code{\link{cond_boot}} and \code{\link{approx_deriv}}
#' @family IND~pressure modelling functions
#'
#' @export
#'
#' @examples
#' # Using some models of the Baltic Sea demo data
#' init_tbl <- ind_init_ex[ind_init_ex$id %in% c(5,9,48,75), ]
#' mod_tbl <- merge_models_ex[merge_models_ex$id  %in% c(5,9,48,75), ]
#' deriv_tbl <- calc_deriv(init_tbl=init_tbl, mod_tbl=mod_tbl,
#'   n_boot = 40, par_comp = FALSE, seed=1)
calc_deriv <- function(init_tbl, mod_tbl, edf_filter = 1.5,
  p_val_filter = 0.05, excl_outlier = FALSE, method = "cond_boot",
	 n_boot = 200, ci_boot = 0.95, ci_prop_se = 25,
  par_comp = FALSE, no_clust = NULL, seed = NULL) {

  # Data input validation ----------------------------

  pos_int <- function(x) {
    if (is.numeric(x)) {
      if (x > 0)
        z <- TRUE else z <- FALSE
    } else {
      z <- FALSE
    }
    return(z)
  }

  if (missing(init_tbl)) {
	 	stop("Argument 'init_tbl' is missing.")
  }
  if (missing(mod_tbl)) {
	 	stop("Argument 'mod_tbl' is missing.")
	 }

  # Check input tibbles
	 init_tbl <- check_input_tbl(
	 	 init_tbl, tbl_name = "init_tbl", parent_func = "ind_init()",
	 	 var_to_check = c("id", "ind", "press", "ind_train", "press_train", "time_train",
	 	 	 "ind_test", "press_test", "time_test", "train_na"),
	 	 dt_to_check = c("integer", "character", "character", rep("list", 7))
	 )
	 mod_tbl <- check_input_tbl(
				mod_tbl, tbl_name = "mod_tbl", parent_func = "model_gam() or model_gamm()/select_model()",
				var_to_check = c("id", "ind", "press", "corrstruc","edf", "p_val", "model"),
				dt_to_check = c("integer", "character", "character", "character", "numeric", "numeric",
					 "list")
		)

  # Test if there are any ids with NAs in models (if,
  # e.g., GAMMs were manually selected and
  # convergence errors occurred)
  if (any(is.na(mod_tbl$model))) {
    stop(paste0("The following ids have missing models: ",
      paste0(mod_tbl$id[is.na(mod_tbl$model)],
        collapse = ", ")))
  }

  # Check if init_tbl represents the same full set or
  # subset of IND-pressure combinations than mod_tbl
  # and otherwise filter for mod_tbl$id (if there is
  # any id missing in init_tbl return error message)
  # and sort in the same order
  if (!identical(init_tbl$id, mod_tbl$id)) {
    if (all(mod_tbl$id %in% init_tbl$id)) {
      init_tbl <- init_tbl[match(mod_tbl$id, init_tbl$id), ]
    	  # (match() with mod_tbl as first argument makes
       # sure only those in the same order are selected)
    } else {
      stop("Not all ids in mod_tbl are provided in init_tbl.")
    }
  }

  # Test if there are any ids with NAs in models (if GAMMs manually selected and convergence errors occurred)
  if (any(is.na(mod_tbl$model))) {
  	stop(paste0("The following ids have missing models: ",
  		paste0(mod_tbl$id[is.na(mod_tbl$model)], collapse = ", ")))
  }

  # Correct edf_filter (>1)?
  if (is.null(edf_filter)) {
    stop("The edf_filter value must be a single numeric value greater than 1.")
  } else {
    if (!is.numeric(edf_filter) | (edf_filter <= 1)) {
      stop("The edf_filter value must be a single numeric value greater than 1.")
    }
  }

  # Correct p_val_filter (between 0 and 1)?
  if (is.null(p_val_filter)) {
    stop("The p_val_filter value must be a single numeric value between 0 and 1.")
  } else {
    if (!is.numeric(p_val_filter) | (p_val_filter <
      0) | (p_val_filter > 1)) {
      stop("The p_val_filter value must be a numeric value between 0 and 1.")
    }
  }

  # Correct ci_boot value (between 0 and 1)?
  if (is.null(ci_boot)) {
    stop("The ci_boot value must be a single numeric value between 0 and 1.")
  } else {
    if (any(!is.numeric(ci_boot) | (ci_boot) < 0 | (ci_boot > 1))) {
      stop("The ci_boot value  value must be a numeric value between 0 and 1.")
    }
  }

  # Correct method (2 options)?
  if (is.null(method)) {
    stop("The method must be either 'conditional bootstrap' or 'approx_deriv'.")
  } else {
    if (!method %in% c("cond_boot",
      "approx_deriv")) {
      stop("The method must be either 'cond_boot' or 'approx_deriv'.")
    }
  }

  # Applies only to the conditional bootstrap
  if (method == "cond_boot") {
    # par_comp logical and no_clust and seed positive
    # integers?
    if (method == "cond_boot" & !is.logical(par_comp)) {
      stop("Specify whether you want the conditional bootstrap computed in parallel (par_comp = TRUE) or not (FALSE).")
    }

    if (isTRUE(par_comp) & !pos_int(no_clust)) {
      if (!is.null(no_clust)) {
        stop("The number of cores must be a positive integer or NULL (default, which uses one less than exisiting cores).")
      }
    } else {
      # In case not an integer
      if (isTRUE(par_comp) & pos_int(no_clust))
        no_clust <- floor(no_clust)
    }

    if (!is.null(seed) & !pos_int(seed)) {
      stop("The seed number has to be NULL or a positive integer.")
    }
  }

  if ((!"excl_outlier" %in% names(mod_tbl)) &
    isTRUE(excl_outlier)) {
    stop("There is no column 'excl_outlier'. Please set excl_outlier to FALSE!")
  }
	 # As the column 'excl_outlier' is later needed, add here
	 #  list of NULLs
	 if ((!"excl_outlier" %in% names(mod_tbl)) &
    excl_outlier == FALSE)  {
    mod_tbl$excl_outlier <- vector("list", length = length(mod_tbl$id))
	 }


  # -------------------------------------------------------

  # Filter mod_tbl and apply the respective deriv
  # method to filtered ids

  if (nrow(mod_tbl[mod_tbl$edf > edf_filter & mod_tbl$p_val <=
    p_val_filter, ]) == 0) {

    # Alternative output as no derivatives have to be
    # calculated --> just add prop = 1.00
    alter_output <- mod_tbl
    alter_output$prop <- 1
    alter_output <- sort_output_tbl(alter_output)
    message("None of the models has an edf > edf_filter and a p_val <= p_val filter, hence, the prop value will be 1 for all.")
    return(alter_output)

  } else {

    # Divide mod_tbl/init_tbl in filtered and
    # unfiltered ids
    filt <- mod_tbl[mod_tbl$edf > edf_filter &
      mod_tbl$p_val <= p_val_filter, ]
    unfilt <- mod_tbl[!mod_tbl$id %in% filt$id, ]
    filt_init <- init_tbl[init_tbl$id %in% filt$id, ]

    # Assign prop of NA or 1.00 to unfiltered ids (if not
    # (NA if not sign.)
    if (!nrow(unfilt) == 0) {
    	unfilt$prop <- NA
    	unfilt$prop[unfilt$p_val <= p_val_filter] <- 1.0
    }

    # Apply method to filtered ids (functions work on
    # entire tibble)
    if (method == "cond_boot") {
      filt_deriv <- cond_boot(init_tbl = filt_init,
        mod_tbl = filt, excl_outlier = excl_outlier,
        n_boot = n_boot, ci = ci_boot,
        par_comp = par_comp, no_clust = no_clust,
        seed = seed)
    } else {
      filt_deriv <- approx_deriv(mod_tbl = filt, init_tbl = filt_init,
        ci_prop_se = ci_prop_se)
    }


    # Calculate significance for derivatives -------------
    filt_deriv$zero_in_conf <- purrr::map2(.x = filt_deriv$deriv1_ci_low,
      .y = filt_deriv$deriv1_ci_up, function(x,
        y) ifelse(x < 0 & y > 0, TRUE, FALSE))

    filt_deriv$zic_start_end <- purrr::map(.x = filt_deriv$zero_in_conf,
      x_range)

    filt_deriv$prop <- purrr::map_dbl(.x = filt_deriv$zic_start_end,
      ~round((1 - sum(.x)/length(.x)), digits = 2))


    # Merge filt and unfilt tibbles -------------------
    output_tbl <- dplyr::bind_rows(unfilt, filt_deriv)
    # Arrange cols and rows
    output_tbl <- sort_output_tbl(output_tbl)
    output_tbl <- dplyr::arrange_(output_tbl, .dots= "id")

    # End of else condition
    return(output_tbl)
  }

  ### END OF FUNCTION
}

# Internal helper function -------------------------------------

# Function to identify boundary areas where the zero-line is
# within the confidence intervals
x_range <- function(vec) {
  n <- length(vec)
  temp_for_if_break <- vector(length = n)
  for (i in 1:n) {
    if (vec[i] == FALSE) {
      break
    }
    temp_for_if_break[i] <- TRUE
  }
  for (j in n:1) {
    if (vec[j] == FALSE) {
      break
    }
    temp_for_if_break[j] <- TRUE
  }

  return(temp_for_if_break)
}

